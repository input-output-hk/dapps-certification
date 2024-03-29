{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Plutus.Certification.Server.Instance where

import Conduit
import Control.Monad.Catch
import Servant
import Servant.Client as Client
import Control.Monad.State.Strict
import Observe.Event
import Observe.Event.BackendModification
import Network.URI
import Plutus.Certification.API as API
import Plutus.Certification.GitHubClient
import Control.Monad.Except
import Data.Time.LocalTime
import Data.Maybe
import Data.Aeson
import Network.HTTP.Client hiding (Proxy,parseUrl)

import Network.HTTP.Types
import Control.Applicative
import Data.Text as Text hiding (elem,replicate, last,words,replicate)
import Data.Text.Encoding
import Plutus.Certification.WalletClient (WalletArgs(walletCertificationPrice))
import IOHK.Certification.Interface hiding (Status)
import Plutus.Certification.Server.Internal as I

import Servant.Server.Experimental.Auth (AuthServerData)
import Data.Time (addUTCTime)
import Plutus.Certification.JWT (jwtEncode, JWTConfig(..))
import IOHK.Certification.SignatureVerification as SV
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Text.Read (readMaybe)
import Data.HashSet as HashSet
import Data.List as List
import IOHK.Certification.Persistence (FeatureType(..))
import Plutus.Certification.ProfileWallet as PW
import Data.Functor
import Plutus.Certification.Metadata
import Control.Monad.Reader (ReaderT(..))
import Control.Concurrent (MVar, takeMVar, putMVar)
import Data.Set (fromList)
import Plutus.Certification.Persistence.Instrumentation as WDB hiding (CreateRun)
import System.FilePath

import qualified Data.ByteString.Lazy.Char8 as LSB
import qualified Paths_plutus_certification as Package
import qualified Plutus.Certification.WalletClient as Wallet
import qualified IOHK.Certification.Persistence as DB
import qualified IOHK.Certification.Persistence.API.SQLite as DB
import qualified Plutus.Certification.Web3StorageClient  as IPFS
import Plutus.Certification.Metrics
import qualified Plutus.Certification.Htmx as Htmx

hoistServerCaps :: (Monad m) => (forall x . m x -> n x) -> ServerCaps m r -> ServerCaps n r
hoistServerCaps nt (ServerCaps {..}) = ServerCaps
  { submitJob = \mods certifyArgs ghAccessTokenM -> nt . submitJob mods certifyArgs ghAccessTokenM
  , getStatus = \mods -> nt . getStatus mods
  , abortRuns = \mods -> nt . abortRuns mods
  , getLogs = \mods act -> transPipe nt . getLogs mods act
  }

data GitHubCredentials = GitHubCredentials
  { githubClientId :: !Text
  , githubClientSecret :: !Text
  } deriving (Show, Eq )

-- | A type for server arguments including the wallet arguments
-- and the github access token as optional
data ServerArgs m r = ServerArgs
  { serverCaps :: !(ServerCaps m r)
  , serverWalletArgs :: !Wallet.WalletArgs
  , serverGithubToken :: !(Maybe GitHubAccessToken)
  , serverJWTConfig :: !(Maybe JWTConfig)
  , serverEventBackend :: !(EventBackend m r ServerEventSelector)
  , serverSigningTimeout :: !Seconds
  , serverWhitelist :: !(Maybe Whitelist)
  , serverGitHubCredentials :: !(Maybe GitHubCredentials)
  , serverValidateSubscriptions :: Bool
  , serverAdaUsdPrice :: m (Maybe DB.AdaUsdPrice)
  , serverAddressRotation :: MVar AddressRotation
  , serverVat :: DB.VatPercentage
  , serverInvoicesFolder :: FilePath
  -- TODO: maybe replace the 'server' prefix
  , withDb :: forall a k. (MonadIO k, MonadMask k)
           -- TODO: Removing DB.Backend n ~ DB.SQLite it's the starting point
           -- when we switch to multi DB backend support
           => (forall n. (DB.MonadSelda n,MonadMask n, DB.Backend n ~ DB.SQLite) => n a)
           -> k a
  }

type Whitelist = HashSet Text

-- | verify if the address is whitelisted
verifyWhiteList :: MonadError ServerError m => Maybe (HashSet Text) -> Text -> m ()
verifyWhiteList Nothing _ = pure ()
verifyWhiteList (Just whitelist) addr =
  unless (HashSet.member addr whitelist) $ throw401 "address not whitelisted"
  where
  throw401 msg = throwError $ err401 { errBody = msg }

type Seconds = Integer

-- | An implementation of 'API'
server :: ( MonadMask m
          , MonadIO m
          , MonadError ServerError m
          -- constraint for the auth server data to be equal to the tuple
          -- of profile id and user address
          , AuthServerData (AuthProtect auth) ~ (DB.ProfileId,DB.ProfileWalletAddress)
          )
       => ServerArgs m r
       -> ServerT (API auth) m
server ServerArgs{..} = NamedAPI
  { version = withEvent eb Version . const . pure $ VersionV1 Package.version
  , versionHead = withEvent eb Version . const $ pure NoContent
  , walletAddress = withEvent eb I.WalletAddress . const $ pure serverWalletArgs.walletAddress
  , createRunOnCurrentProfile = \options (profileId,_) -> do
      -- ensure the profile has an active feature for L1Run
      validateFeature L1Run profileId
      createRun' options profileId
  , createRun = \options profileId (ownerId,_) -> do
      hasRole <- withDb (DB.hasAtLeastUserRole ownerId DB.Support)
      case (hasRole,ownerId == profileId) of
        (False,False) -> throwError err403 { errBody =
          "You don't have the necessary rights to create a run for this profile" }
        (False,True) -> validateFeature L1Run ownerId
        (True,_) -> pure ()
      createRun' options profileId
  , getRun = \rid@RunID{..} -> withEvent eb GetRun \ev ->
      getStatus (setAncestor $ reference ev) rid >>= runDbReader . dbSync uuid
  , getRunDetails = \rid@RunID{..} -> withEvent eb GetRunDetails \ev -> do
      addField ev rid
      -- get the run details from the db
      withDb ( DB.getRun uuid )
        >>= maybeToServerError err404 "Run not found"
  , abortRun = \rid@RunID{..} deleteRun' (profileId,_) -> withEvent eb AbortRun \ev -> do
      addField ev rid

      -- ensure the user has necessary rights to start a run
      -- ( has at least support role or is the owner of the run )
      hasNecessaryRole <- withDb (DB.hasAtLeastUserRole profileId DB.Support)
      unless hasNecessaryRole $
        requireRunIdOwner profileId uuid

      -- abort the run if is still running
      status <- getStatus (setAncestor $ reference ev) rid
      when (toDbStatus status == DB.Queued) $ do

        -- finally abort the run
        abortRuns (setAncestor $ reference ev) rid
        -- if abortion succeeded mark it in the db
        now <- getNow
        void $ runDbReader (WDB.updateFinishedRun eb' uuid False now)

      -- depending on the deleteRun flag either ...
      if deleteRun' == Just True
         -- delete the run from the db
         then void (runDbReader $ WDB.deleteRun eb' uuid)
         -- or just mark it as aborted
         else do
           now <- getNow
           void $ runDbReader $ WDB.markAsAborted eb' uuid now
      pure NoContent

  , getCurrentProfileBalance = profileBalance
  -- elevated version
  , getProfileBalance = impersonateWithAddress DB.Support profileBalance

  , getLogs = \rid afterM actionTypeM -> withEvent eb GetRunLogs \ev -> do
      addField ev rid
      let dropCond = case afterM of
            (Just after) ->
              let afterUtc = zonedTimeToUTC after
              in ((<= afterUtc) . zonedTimeToUTC . time)
            Nothing -> const False
      runConduit
         $ getLogs (setAncestor $ reference ev) actionTypeM rid
        .| (dropWhileC dropCond >> sinkList)
  -- TODO: elevate this to admin
  -- ELEVATE and HERE
  , getCurrentProfileRuns = \(profileId,_) afterM countM ->
      withDb $ DB.getRuns profileId afterM countM
  , getProfileRuns = \profileId afterM countM -> impersonate DB.Support
      (const $ withDb $ DB.getRuns profileId afterM countM) profileId
  , updateCurrentProfile = updateProfile
  , updateProfile = impersonateWithAddress DB.Support . updateProfile

  , getCurrentProfile = \(profileId,_) -> getProfileDTO profileId
  , getProfile = impersonateWithAddress DB.Support (\(profileId,_) -> getProfileDTO profileId)

  -- TODO: There might be an issue if more than one certification is started at
  -- the same time for the same Run:
  -- Multiple transactions are going to be broadcasted at the same time and
  -- therefore we are going to pay multiple fees.
  -- We have to somehow create a lock mechanism for every certification per run
  , createCertification = \(profileId,_) rid@RunID{..} certInput dryRun ->
    withEvent eb CreateL1Certification \ev -> do

    addField ev (CreateL1CertificationRunID rid)
    addField ev (CreateL1CertificationDryRun (dryRun == Just True))
    -- ensure runId belongs to the owner
    requireRunIdOwner profileId uuid

    -- get the runId report
    status <- getStatus (setAncestor $ reference ev) rid

    -- sync the run with the db and return the db-run information
    DB.Run{runStatus,reportContentId,withCustomOptions} <- getRunAndSync rid status

    -- if withCustomOptions is true, throw 405
    when withCustomOptions $ throwError err405 { errBody = "Run has custom options" }

    subject <- getProfileDAppSubject profileId

    let getInput reportUrls = AuditorCertificationInput certInput reportUrls subject certLevel
    -- create the certification metadata
    case dryRun of
      -- if it's a dry run, just return the metadata
      Just True ->do
        let reportUrls = case reportContentId of
              Just rCid -> [ReportURL $ parseURIUnsafe ("ipfs://" <> Text.unpack rCid)]
              Nothing -> []
        let auditorCertificationInput = getInput reportUrls
        catch (createDraftMetadata auditorCertificationInput True) handleException
      -- otherwise push both the metadata and the run report to the ipfs and return the full metadata
      _ -> do
        let certResultM = toCertificationResult status
        -- upload the report to ipfs or use the existing ipfs cid
        ipfsCid <- case reportContentId of
          Just rCid -> pure (DB.IpfsCid rCid)
          Nothing -> uploadRunReportToIpfs ev runStatus certResultM uuid
        let reportUrl = ReportURL $ parseURIUnsafe ("ipfs://" <> Text.unpack (DB.ipfsCid ipfsCid))
        let auditorCertificationInput = getInput [reportUrl]

        (fullMetadata,metadataIpfs) <- catch (createMetadataAndPushToIpfs auditorCertificationInput) handleException
        addField ev (CreateL1CertificationMetadataIpfsCid metadataIpfs)
        pure fullMetadata

  , getRepositoryInfo = \owner repo apiGhAccessTokenM -> withEvent eb GetRepoInfo \ev -> do
    addField ev (GetRepoInfoOwner owner)
    addField ev (GetRepoInfoRepo repo)
    let ghAccessTokenM = unApiGitHubAccessToken <$> apiGhAccessTokenM
        -- if there is no github access token, we use the default one
        -- provided from arguments
        ghAccessTokenM' =  ghAccessTokenM <|> serverGithubToken
    liftIO ( getRepoInfo ghAccessTokenM' owner repo ) >>= fromClientResponse

  , login = \LoginBody{..} -> whenJWTProvided \JWTConfig{..} -> withEvent eb Login \ev -> do
      addField ev address
      now <- getNow
      -- verify whitelist
      verifyWhiteList serverWhitelist address
      -- ensure the profile exists
      (pid,userAddress) <- runDbReader (ensureProfileFromBs $ encodeUtf8 address)
      --verify the wallet signature validation
      verifySignature key signature address
      -- verify the message timestamp
      verifyMessageTimeStamp signature

      let -- minimum between the expiration time and the jwtExpirationSeconds
          expiration' = maybe jwtExpirationSeconds (min jwtExpirationSeconds) expiration
          expiresAt = addUTCTime (fromIntegral expiration') now

      -- encode jwt with with expiration time
      pure $ jwtEncode jwtSecret expiresAt (DB.fromId pid, userAddress)
  , serverTimestamp = withEvent eb Version (const $ round . utcTimeToPOSIXSeconds <$> getNow)

  , generateGitHubToken = \code -> withEvent eb GenerateGitHubToken \ev -> do
      case serverGitHubCredentials of
        Nothing -> throwError err404 { errBody = "GitHub credentials not configured" }
        Just GitHubCredentials{..} -> do
          respE <- liftIO $ generateGithubAccessToken githubClientId githubClientSecret code
          either (\err -> do
           addField ev (GenerateGitHubTokenError $ show err)
           -- let's hide the error from the user from security reasons
           throwError err400 { errBody = "Invalid code or something went wrong" }
           ) pure respE

  , getGitHubClientId = withEvent eb GetGitHubClientId \_ ->
      case serverGitHubCredentials of
        Nothing -> throwError err404 { errBody = "GitHub credentials not configured" }
        Just GitHubCredentials{..} -> pure githubClientId

  , getCurrentProfileSubscriptions = \justEnabled (pid,_) ->
      profileSubscription justEnabled pid
  , getProfileSubscriptions =
      impersonate DB.Support . profileSubscription

  , subscribe = \(profileId,_) tierIdInt -> withEvent eb Subscribe \ev -> do
    -- check if the profile is ready for invoicing
    isReady <- withDb $ DB.isProfileReadyForInvoicing profileId
    unless isReady $
      throwError err405 { errBody = "Profile is not ready for invoicing" }
    let tierId = DB.toId tierIdInt
    addField ev $ SubscribeFieldProfileId profileId
    addField ev $ SubscribeFieldTierId tierId
    now <- getNow
    adaUsdPrice' <- getAdaUsdPrice'
    ret <- runDbReader (WDB.createSubscription eb' now profileId tierId adaUsdPrice')
    forM_ ret $ \dto -> addField ev $ SubscribeFieldSubscriptionId (dto.subscriptionDtoId)
    maybeToServerError err404 "Tier not found" ret

  , cancelCurrentProfilePendingSubscriptions = \(pid,_) ->
      cancelProfilePendingSubscriptions pid
  , cancelProfilePendingSubscriptions =
      impersonate DB.Admin cancelProfilePendingSubscriptions

  , getAllTiers = withEvent eb GetAllTiers \ev -> do
    tiers <- withDb DB.getAllTiers
    addField ev (List.length tiers)
    -- TODO: remove when you want to activate all tiers
    -- remove the Developer tier from the list
    let filteredTiers = List.filter (\t -> t.tierDtoTier.tierType  /= DB.Developer ) tiers
    pure filteredTiers

  , getCurrentProfileActiveFeatures = \(profileId,_)
      -> profileActiveFeatures profileId
  , getProfileActiveFeatures = impersonate DB.Support profileActiveFeatures

  , getAdaUsdPrice = withEvent eb GetAdaUsdPrice \ev -> do
    adaUsdPrice' <- getAdaUsdPrice'
    addField ev adaUsdPrice'
    pure adaUsdPrice'

  , createAuditorReport = \dryRun reportInput (profileId,_) ->
    withEvent eb CreateAuditorReport \ev -> do
      validateFeature L2UploadReport profileId
      addField ev $ CreateAuditorReportFieldProfileId profileId
      addField ev $ CreateAuditorReportDryRun (dryRun == Just True)
      case dryRun of
        Just True -> catch (createDraftMetadata reportInput False) handleException
        _ -> do
          (fullMetadata,ipfs) <- catch
            (createMetadataAndPushToIpfs reportInput)
            handleException
          addField ev $ CreateAuditorReportIpfsCid ipfs
          now <- getNow
          _ <- withDb $ DB.addAuditorReportEvent $ DB.AuditorReportEvent
            { areId = undefined -- this will be replaced by persistence API
            , areProfileId = profileId
            , areCertLevel = reportInput.certificationLevel
            , areCreatedAt = now
            , areOffchainContentId = ipfs.ipfsCid
            }
          pure fullMetadata

  , getCurrentProfileWalletAddress = \(pid,_) -> profileWalletAddress pid
  , getProfileWalletAddress = impersonate DB.Support profileWalletAddress

  , updateProfileRoles = \profileId allRoles (ownerId,_)  ->
    withEvent eb UpdateProfileRoles \ev -> do
      addField ev $ UpdateProfileRolesFieldProfileId profileId
      addField ev $ UpdateProfileRolesFieldRoles allRoles

      -- filter out the NoRole from the list
      let filteredRoles = List.filter (/= DB.NoRole) allRoles

      verifyRole ownerId DB.Admin
      void $ runDbReader (WDB.updateUserRoles eb' profileId (Data.Set.fromList filteredRoles))
      pure NoContent

  , getCurrentProfileRoles = \(pid,_) -> getProfileRoles pid
  , getProfileRoles = impersonate DB.Support getProfileRoles

  , getAllProfilesByRole = \role (pid,_) ->
    withEvent eb GetAllProfilesByRole \ev -> do
      addField ev role
      verifyRole pid DB.Support
      withDb $ DB.getAllProfilesByRole role

  , getProfilesSummary = \(pid,_) -> withEvent eb GetProfilesSummary \ev -> do
      addField ev pid
      verifyRole pid DB.Support
      withDb DB.getProfilesSummary

  , getRunTimeMetrics = \RunTimeArguments{..} (pid,_) -> withEvent eb GetRunTimeMetrics \ev -> do
      let SlotSelector (start,end) = interval
      verifyRole pid DB.Support
      addField ev $ GetRunTimeMetricsFieldStart start
      addField ev $ GetRunTimeMetricsFieldEnd end
      runs <- withDb $ DB.getRunsInInterval start end
      let runMetrics = List.map (runToMetric start end) runs
      let filteredRuns = case minRunTime of
            Nothing -> runMetrics
            Just minRunTime' -> longRunning minRunTime' runMetrics
      addField ev $ GetRunTimeMetricsFieldRuns (List.length filteredRuns)
      mapM_ (addField ev . GetRunTimeMetricsFieldMinimumTime) minRunTime
      pure filteredRuns

  , getSubscriptionsStartingInIntervalRoute = \(SlotSelector (start,end)) (pid,_) -> withEvent eb GetSubscriptionsStartingInInterval \ev -> do
      verifyRole pid DB.Support
      addField ev $ GetSubscriptionsInIntervalFieldStart start
      addField ev $ GetSubscriptionsInIntervalFieldEnd end
      subs <- withDb $ DB.getSubscriptionsStartingInInterval start end
      addField ev $ GetSubscriptionsInIntervalFieldSubscriptions (List.length subs)
      pure subs

  , getSubscriptionsEndingInIntervalRoute = \(SlotSelector (start,end)) (pid,_) -> withEvent eb GetSubscriptionsEndingInInterval \ev -> do
      verifyRole pid DB.Support
      addField ev $ GetSubscriptionsInIntervalFieldStart start
      addField ev $ GetSubscriptionsInIntervalFieldEnd end
      subs <- withDb $ DB.getSubscriptionsEndingInInterval start end
      addField ev $ GetSubscriptionsInIntervalFieldSubscriptions (List.length subs)
      pure subs

  , getAuditorReportMetrics = \(SlotSelector (start,end)) (pid,_) -> withEvent eb GetAuditorReportMetrics \ev -> do
      verifyRole pid DB.Support
      addField ev $ GetAuditorReportMetricsFieldStart start
      addField ev $ GetAuditorReportMetricsFieldEnd end
      auditorReportEvs <- withDb $ DB.getAuditorReportsInInterval start end
      addField ev $ GetAuditorReportMetricsFieldReports (List.length auditorReportEvs)
      pure auditorReportEvs

  , htmx = homePage
       :<|> htmxPage
       :<|> getProfileSummaryRow
       :<|> transactionsPage
       :<|> billingPage
       :<|> invoiceHtml
       :<|> invoiceFromFile

  , downloadInvoice = \ (ownerId,_) invId -> do
      invoiceDtoM <- withDb $ DB.getInvoice invId
      hasRole <- withDb (DB.hasAtLeastUserRole ownerId DB.Support)
      -- if doesn't have the role for support, check if it's the owner of the invoice
      unless hasRole $
        case invoiceDtoM of
          Nothing -> throwError err404 { errBody = "Invoice not found" }
          Just (DB.InvoiceDTO DB.Invoice{invProfileId} _) -> do
            unless (invProfileId == ownerId) $
              throwError err403 {errBody = "You don't have the required rights"}
      invoiceFromFile invId

  , getProfileInvoices = impersonate DB.Support \profileId -> withEvent eb GetProfileInvoices \ev -> do
      addField ev (GetProfileInvoicesFieldProfileId profileId)
      invoices  <- withDb $ DB.getProfileInvoices profileId Nothing Nothing
      addField ev (GetProfileInvoicesFieldInvoicesNo (List.length invoices))
      pure invoices

  , getAllInvoices = \(FlexibleSlotSelector (from,to)) (ownerId,_) -> withEvent eb GetAllInvoices \ev -> do
      addField ev $ GetAllInvoicesFieldFrom from
      addField ev $ GetAllInvoicesFieldTo to
      verifyRole ownerId DB.Support
      invoices <- withDb $ DB.getAllInvoices from to
      addField ev $ GetAllInvoicesFieldInvoicesNo (List.length invoices)
      pure invoices

  , cancelInvoice = \invId (ownerId,_) -> withEvent eb CancelInvoice \ev -> do
      addField ev $ CancelInvoiceFieldSourceInvoiceId invId
      addField ev $ CancelInvoiceFieldByProfileId ownerId
      verifyRole ownerId DB.Support
      cancellationResult <- withDb $ DB.cancelInvoice invId
      case cancellationResult of
        DB.InvoiceNotFound -> throwError err404 { errBody = "Invoice not found" }
        DB.InvoiceAlreadyCanceled -> throwError err405 { errBody = "Invoice already canceled" }
        DB.InvoiceCanceled inv -> do
          addField ev $ CancelInvoiceFieldResultingInvoiceId (inv.invDtoParent.invId)
          addField ev $ CancelInvoiceFieldForProfileId (inv.invDtoParent.invProfileId)
          --TODO: generate invoice pdf
          pure inv

  , createInvoice = \profileId invoiceBody (ownerId,_) -> withEvent eb CreateInvoice \ev -> do
      addField ev $ CreateInvoiceFieldByProfileId ownerId
      addField ev $ CreateInvoiceFieldForProfileId profileId
      verifyRole ownerId DB.Support
      adaUsdPrice <- getAdaUsdPrice'
      addField ev $ CreateInvoiceFieldAdaUsdPrice adaUsdPrice
      now <- getNow
      invResult <- withDb $ DB.createInvoice profileId now adaUsdPrice invoiceBody
      --TODO: generate invoice pdf
      case invResult of
        DB.CreateInvoiceResultProfileNotFound ->
          throwError err404 { errBody = "Profile not found" }
        DB.CreateInvoiceResultRequiredFieldMissing field -> do
          let fieldBs =LSB.fromStrict $ encodeUtf8 field
          throwError err400 { errBody = "Required field missing: " <> fieldBs }
        DB.CreateInvoiceResultInvoice inv -> do
          addField ev $ CreateInvoiceFieldInvoiceId (inv.invDtoParent.invId)
          pure inv

  , createSubscriptionInvoice = \subId (ownerId,_) -> withEvent eb CreateSubscriptionInvoice \ev -> do
      addField ev $ CreateSubscriptionInvoiceFieldByProfileId ownerId
      addField ev $ CreateSubscriptionInvoiceFieldSubscriptionId subId
      verifyRole ownerId DB.Support
      result <- withDb $ DB.createSubscriptionInvoice subId serverVat
      case result of
        DB.SubscriptionNotFound ->
          throwError err404 { errBody = "Subscription not found" }
        DB.SubscriptionStatusNotCompatible ->
          throwError err405 { errBody = "Subscription status not compatible" }
        DB.SubscriptionAlreadyInvoiced ->
          throwError err405 { errBody = "Subscription already invoiced" }
        DB.SubscriptionMissingProfileData field ->
          throwError err400 { errBody = "Subscription missing profile data: " <> LSB.pack field }
        DB.SubscriptionInvoiced inv@(DB.InvoiceDTO DB.Invoice{..}  _) -> do
          addField ev $ CreateSubscriptionInvoiceFieldInvoiceId invId
          addField ev $ CreateSubscriptionInvoiceFieldForProfileId invProfileId
          pure inv
  }
  where
    invoiceFromFile invId = do
      let strInvId = show (DB.fromId invId)
      pdf <- liftIO $ LSB.readFile $ serverInvoicesFolder </> strInvId <> ".pdf"
      let contentDisposition = "inline; filename=" <> strInvId <> ".pdf"
      pure $ addHeader "application/pdf"
           $ addHeader "Binary"
           $ addHeader contentDisposition
           $ PdfBytesString pdf
    homePage = pure $ RawHtml Htmx.homePage
    transactionsPage = pure $ RawHtml Htmx.transactionsPage
    billingPage = do
      -- get all invoices
      invoices <- withDb $ DB.getAllInvoices Nothing Nothing
      pure $ RawHtml $ Htmx.billingPage invoices
    invoiceHtml invId = do
      invoice <- withDb (DB.getInvoice invId)
      case invoice of
        Just inv -> pure $ RawHtml $ Htmx.renderInvoice inv
        Nothing -> throwError err404 { errBody = "Invoice not found" }

    htmxPage = withDb DB.getProfilesSummary <&> RawHtml . Htmx.accountsPage
    getProfileSummaryRow profileId =
      withDb (DB.getProfileSummary profileId) >>= \case
        Nothing -> throwError err404 { errBody = "Profile not found" }
        Just summary -> pure $ RawHtml $ Htmx.renderAccountsTrDetailed summary
    createRun' CreateRunOptions{..} profileId = withEvent eb CreateRun \ev -> do
      -- get the flake ref and the github token from the db
      (fref,profileAccessToken) <- getFlakeRefAndAccessToken profileId croCommitOrBranch

      -- get one of the github tokens. from the profile if it has one
      -- or from the server arguments
      let githubToken' =  profileAccessToken <|> serverGithubToken
      --
      -- ensure the ref is in the right format before starting the job
      (commitDate,commitHash) <- getCommitDateAndHash githubToken' fref
      addField ev $ CreateRunRef fref

      -- submit the job
      res <- submitJob (setAncestor $ reference ev) croCertArgs githubToken' fref
      addField ev $ CreateRunID res

      -- insert the run info in the db
      createDbRun fref profileId res commitDate commitHash (croCertArgs /= DefaultCertifyArgs)
      pure res

  --------------------------------------------------------------------------------
  -- | common profile handlers
    getProfileRoles profileId = withEvent eb GetProfileRoles \ev -> do
      addField ev profileId
      withDb $ DB.getUserRoles profileId

    profileActiveFeatures profileId = withEvent eb GetActiveFeatures \ev -> do
      addField ev $ GetActiveFeaturesFieldProfileId profileId
      now <- getNow
      featureTypes <- withDb $ DB.getCurrentFeatures profileId now
      addField ev $ GetActiveFeaturesFieldFeatures featureTypes
      pure featureTypes

    cancelProfilePendingSubscriptions profileId =
      withEvent eb CancelProfilePendingSubscriptions \ev -> do
        addField ev $ CancelProfilePendingSubscriptionsFieldProfileId profileId
        withDb (DB.cancelPendingSubscription profileId)

    profileSubscription justEnabled profileId =
      withEvent eb GetProfileSubscriptions \ev -> do
        addField ev profileId
        withDb (DB.getProfileSubscriptions profileId (fromMaybe False justEnabled))

    profileWalletAddress profileId =
      withEvent eb GetProfileWalletAddress \ev -> do
      addField ev profileId

      -- first check the db
      walletM <- (id <=< fmap snd) <$> withDb ( DB.getProfileWallet profileId )

      -- second, if there is nothing in the db try to get
      case walletM of
        Just (DB.ProfileWallet _ address status _) ->
          pure $ Just (status, address)
        Nothing -> do
          resp <- Wallet.getWalletAddresses wallet (Just Wallet.Unused)
          case resp of
            Right unusedAddressesInfo -> do
              let unusedAddresses = fmap (PW.WalletAddress . (.addressId)) unusedAddressesInfo
              (walletAddressM,newRotation) <- liftIO (takeMVar serverAddressRotation)
                <&> getTemporarilyWalletAddress unusedAddresses profileId
              liftIO $ putMVar serverAddressRotation newRotation
              pure $ fmap ((DB.Overlapping,) . unWalletAddress) walletAddressM
            Left err -> withEvent eb InternalError $ \ev' -> do
              addField ev' (show err)
              throwError $ err500 {errBody = LSB.pack $ show resp}

    --TODO: add event
    updateProfile (ProfileBody profile dapp) (profileId,ownerAddress) =
      withEvent eb UpdateProfile \ev ->
      do
        addField ev profileId
        let dappId = profileId
            -- replace dappId from the serialization
            dappM = fmap (\(DB.DApp{dappId=_,..}) -> DB.DApp{..}) dapp
            -- replace profileId and ownerAddress from the serialization
            DB.Profile{ownerAddress=_,profileId=_,..} = profile
        --NOTE: ownerAddress is get from the JWT token
        --not from the request body
        _ <- runDbReader $ WDB.upsertProfile eb' (DB.Profile{..}) dappM

        -- it's safe to call partial function fromJust
        fromJust <$> withDb (DB.getProfile profileId)

    eb' = hoistEventBackend (ReaderT . const) (narrowEventBackend InjectPersistenceSel eb)
    profileBalance (profileId,ownerAddress) =
      withEvent eb GetProfileBalance \ev -> do
        addField ev profileId
        fromMaybe 0 <$> withDb (DB.getProfileBalance ownerAddress)

    ----------------------------------------------------------------------------
    -- | authorization handlers
    verifyRole :: ( MonadMask m
                  , MonadIO m
                  , MonadError ServerError m
                  )
                  => DB.ProfileId
                  -> DB.UserRole
                  -> m ()
    verifyRole profileId role = do
      hasRole <- withDb (DB.hasAtLeastUserRole profileId role)
      unless hasRole $ throwError err403 {errBody = "You don't have the required rights"}

    impersonateWithAddress :: ( MonadMask m
                              , MonadIO m
                              , MonadError ServerError m
                              )
                           => DB.UserRole
                           -> ((DB.ProfileId, ProfileWalletAddress) -> m a)
                           -> DB.ProfileId
                           -> (DB.ProfileId, ProfileWalletAddress)
                           -> m a
    impersonateWithAddress role f impersonatedId (ownerId,address')
      | impersonatedId == ownerId = f (ownerId,address')
      | otherwise = do
        impersonation <- withDb (DB.verifyImpersonation ownerId role impersonatedId)
        case (impersonation, impersonatedId == ownerId) of
          (DB.ImpersonationNotAllowed, _ )-> throwError err403
          (DB.ImpersonationNotFound , _)-> throwError err404
          (DB.ImpersonationProfile (pid,address), _)-> f (pid,address)

    -- faster if we don't need the address
    impersonate :: ( MonadMask m
                   , MonadIO m
                   , MonadError ServerError m
                   )
                => DB.UserRole
                -> (DB.ProfileId -> m a)
                -> DB.ProfileId
                -> (DB.ProfileId, ProfileWalletAddress)
                -> m a
    impersonate role f profileId (ownerId,_)
      | profileId == ownerId = f profileId
      | otherwise = do
          hasRole <- withDb (DB.hasAtLeastUserRole ownerId role)
          if hasRole then f profileId else throwError err403

    uploadRunReportToIpfs ev runStatus certResultM uuid = do
      -- ensure the run is finished
      unless (runStatus == DB.Succeeded) $
        throwError err403 { errBody = "Transaction status not fit for certification" }
      -- upload the report to ipfs
      (IPFS.UploadResponse ipfsCid _) <- maybe
        (throwError $ err403 { errBody = "can't upload report to ipfs" })
        uploadToIpfs certResultM
      --
      -- mark the run as ready for certification
      now <- getNow
      _ <- runDbReader (WDB.markAsReadyForCertification eb' uuid ipfsCid now)

      addField ev (CreateL1CertificationReportIpfsCid ipfsCid)
      pure ipfsCid

    -- TODO: for the moment we use L0 for all the certificates
    certLevel = DB.L0
    wallet = Wallet.realClient serverWalletArgs
    handleException :: (MonadError ServerError m ) =>  SomeException -> m a
    handleException e = do
      throwError err400 { errBody = LSB.pack $ show e }

    getAdaUsdPrice' =
      serverAdaUsdPrice >>= maybeToServerError err500 "Can't get ada usd price"

    validateFeature featureType profileId = do
      -- ensure the profile has an active feauture for L1Run
      when serverValidateSubscriptions $ do
        -- bypass subscription validation if it's an internal user
        hasRole <- withDb (DB.hasAtLeastUserRole profileId DB.Support)
        unless hasRole $ do
          now <- getNow
          featureTypes <- withDb ( DB.getCurrentFeatures profileId now )
          unless (featureType `elem` featureTypes) $
            throwError err403 { errBody = "You don't have the required subscription" }

    fromClientResponse = \case
      Left err -> throwError $ serverErrorFromClientError err
      Right a -> pure a

    serverErrorFromClientError :: ClientError -> ServerError
    serverErrorFromClientError clientResponse =
      case clientResponse of
        FailureResponse _ resp -> errorFromClientResponse resp
        DecodeFailure _ resp -> err500WithResp resp
        UnsupportedContentType _ resp -> err500WithResp resp
        InvalidContentTypeHeader resp -> err500WithResp resp
        ConnectionError _ -> err500 {errBody = "Connection error"}
      where
      err500WithResp resp = err500 {errBody = LSB.pack $ show resp}

    errorFromClientResponse :: Client.Response -> ServerError
    errorFromClientResponse resp =
      let (Status code msg) = responseStatusCode resp
          err = ServerError code "GitHub API error" (LSB.fromStrict msg) []
      in err
    -- | unfolds the payload from the signed message
    unfoldMessage bs = case unfoldPayload bs of
      Left err -> throwError err403 { errBody = LSB.pack err }
      Right payload -> pure . decodeUtf8 . unMessage $ payload.message

    -- | extracts the timestamp from the signed message
    extractTimeStampFromMsg bs = do
      msg <- unfoldMessage bs
      case extractTimestamp msg of
        Nothing -> throwError err403 { errBody = "Invalid message format"}
        Just ts -> pure ts

    -- | extracts the timestamp from the signed message and verifies it
    verifyMessageTimeStamp = extractTimeStampFromMsg >=> verifyTimeStamp

    -- | verifies that the timestamp from the signed message is not older than `serverSigningTimeout`
    verifyTimeStamp ts = do
      now <- round . utcTimeToPOSIXSeconds <$> getNow
      let diff = now - ts
      -- if the difference greater than the timeout or the timestamp is in the future
      -- then the message is invalid
      when (diff > serverSigningTimeout || diff < 0) $
        throwError err403 { errBody = "Invalid message timestamp"}

    -- | extracts an int time stamp from a string of form "xxxxxx<<timestamp>>xxxxxx"
    -- the timestamp is enclosed in double angle brackets
    extractTimestamp :: Read b => Text -> Maybe b
    extractTimestamp t = do
      let (_,post) = Text.breakOn "<<" t
      let (ts,post') = Text.breakOn ">>" (Text.drop 2 post)
      guard $ not $ Text.null post'
      readMaybe $ Text.unpack ts

    ServerCaps {..} = serverCaps
    jwtArgs = serverJWTConfig
    eb = serverEventBackend
    verifySignature key signature address =
      let res = verifyCIP30Signature key signature Nothing (Just $ Bech32Address address)
      in either (\err -> throwError err401 { errBody = LSB.pack err })
        pure $ case res of
          Left err -> Left err
          Right False -> Left  "Signature verification failed"
          Right True -> Right ()

    whenJWTProvided handler = case jwtArgs of
      Nothing -> throwError err404
      Just jwtArgs'-> handler jwtArgs'

    uploadToIpfs :: (Monad m, MonadIO m, MonadError ServerError m) => CertificationResult -> m IPFS.UploadResponse
    uploadToIpfs certResultM = do
      resp <- IPFS.uploadReportToIpfs IPFS.apiKey (LSB.toStrict $ encode certResultM)
      case resp of
        Left (IPFS.DecodeFailure _ err) -> throwError err500 { errBody = encode err }
        Left (IPFS.HttpError resp') ->
          let (Status code msg) = responseStatus resp'
              err = ServerError code "IPFS gateway error" (LSB.fromStrict msg) []
          in throwError err
        Right result -> pure result

    getRunAndSync RunID{..} status = do
      run <- withDb (DB.getRun uuid)
        >>= maybeToServerError err404 "No Run"
      _ <- runDbReader $ dbSync uuid status
      pure run

    getFlakeRefAndAccessToken profileId commitOrBranch =
      getProfileDApp profileId >>= flip createFlakeRef commitOrBranch

    eitherToServerError baseHttpError f = either
      (\cerr -> throwError baseHttpError { errBody = f cerr})
      pure

    maybeToServerError baseHttpError msg = maybe
      (throwError baseHttpError { errBody = msg})
      pure

    createFlakeRef DB.DApp{..} CommitOrBranch{..} = do
      when (Text.null dappOwner || Text.null dappRepo )
        $ forbidden "DApp owner or repo are empty"

      let uri = "github:" <> encodeUtf8 dappOwner <> "/" <> encodeUtf8 dappRepo <> "/" <> encodeUtf8 commitOrBranch
      fref <- eitherToServerError
        err400 LSB.pack
        (mimeUnrender (Proxy :: Proxy PlainText) (LSB.fromStrict uri))
      pure (fref,dappGitHubToken)

    forbidden str = throwError $ err403 { errBody = str}

    getProfileDApp profileId = withDb (DB.getProfileDApp profileId)
        >>= withDappNotAvailableMsg

    getProfileDAppSubject profileId = do
      DB.DApp{..} <- withDb (DB.getProfileDApp profileId) >>= withDappNotAvailableMsg
      maybeToServerError err403 "DApp subject not available" dappSubject

    withDappNotAvailableMsg = maybeToServerError err403 "DApp profile data not available"

    getProfileDTO profileId = withDb (DB.getProfile profileId)
        >>= maybeToServerError err404 "Profile not found"

    requireRunIdOwner profileId uuid = do
      -- ensure is the owner of the run
      isOwner <- (== Just profileId) <$> withDb (DB.getRunOwner uuid)
      unless isOwner $ throwError err403

    getCommitDateAndHash githubToken' FlakeRef{..} = do
      (owner,repo,path') <- extractUriSegments uri
      -- TODO: here we might have to use a github token
      -- provided by the user for authorizing our app to access a private repo
      commitInfoE <- liftIO $ getCommitInfo githubToken' owner repo path'
      case commitInfoE of
           Left e -> throwError err400 { errBody = LSB.pack $ show e}
           Right (Commit hash (CommitDetails _ _ (Author _ _ time))) -> pure (time,hash)

    extractUriSegments uri = case pack <$> pathSegments uri of
        owner:repo:path':_ -> pure (owner,repo,path')
        _ -> throwError err400 { errBody = "Wrong flake-ref details"}

    createDbRun FlakeRef{..} profileId res commitDate commitHash withCustomOptions = do
      now <- getNow
      let uriTxt = pack $ uriToString id uri ""
      runDbReader $ WDB.createRun eb' (uuid res) now uriTxt commitDate
        commitHash (serverWalletArgs.walletCertificationPrice) withCustomOptions profileId

    runDbReader :: ReaderT WithDBWrapper m a -> m a
    runDbReader dbWork = runReaderT dbWork (WithDBWrapper withDb)
