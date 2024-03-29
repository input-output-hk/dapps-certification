{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Plutus.Certification.Server.Internal where

import Conduit
import Control.Monad.Catch hiding (Handler)
import Data.ByteString.Char8 as BS hiding (hPutStrLn,foldl')
import Data.Aeson
import Data.Void
import Servant
import Control.Monad.State.Strict
import Observe.Event.BackendModification
import Observe.Event.Render.JSON
import Network.URI
import IOHK.Certification.Interface hiding (Status)
import Servant.Server.Experimental.Auth
import Plutus.Certification.API as API
import Data.Time
import Data.Text as Text hiding (elem,replicate, last)
import Data.Text.Encoding
import Data.UUID
import Control.Monad.Except
import Plutus.Certification.WalletClient (WalletAddress)
import Plutus.Certification.Internal

import qualified IOHK.Certification.Persistence as DB
import Control.Monad.RWS (MonadReader)
import Plutus.Certification.Persistence.Instrumentation (DbSelector, renderPersistenceSelector)

-- | Capabilities needed to run a server for 'API'
data ServerCaps m r = ServerCaps
  { -- | Submit a new certification job
    submitJob :: !(EventBackendModifiers r r -> CertifyArgs -> Maybe GitHubAccessToken -> FlakeRefV1 -> m RunIDV1)
  , -- | Get the status of a job
    getStatus :: !(EventBackendModifiers r r -> RunIDV1 -> m RunStatusV1)
  , -- | Delete all runs associated with a job
    abortRuns :: !(EventBackendModifiers r r -> RunIDV1 -> m ())
  , -- | Get the logs for all runs associated with a job
    getLogs :: !(EventBackendModifiers r r -> Maybe CertificationStage -> RunIDV1 -> ConduitT () RunLog m ())
  }

data CreateRunField
  = CreateRunRef !FlakeRefV1
  | CreateRunID !RunIDV1

data CreateL1CertificationField
  = CreateL1CertificationRunID !RunIDV1
  | CreateL1CertificationReportIpfsCid !DB.IpfsCid
  | CreateL1CertificationMetadataIpfsCid !DB.IpfsCid
  | CreateL1CertificationDryRun !Bool

data GetRepoInfoField
  = GetRepoInfoOwner !Text
  | GetRepoInfoRepo !Text

type Error = String
newtype GenerateGitHubTokenField = GenerateGitHubTokenError Error

data SubscribeField
  = SubscribeFieldTierId !DB.TierId
  | SubscribeFieldProfileId !DB.ProfileId
  | SubscribeFieldSubscriptionId !DB.SubscriptionId

data CancelProfilePendingSubscriptionsField
  = CancelProfilePendingSubscriptionsFieldProfileId !DB.ProfileId
  | CancelProfilePendingSubscriptionsFieldCount !Int

data GetActiveFeaturesField
  = GetActiveFeaturesFieldProfileId !DB.ProfileId
  | GetActiveFeaturesFieldFeatures ![DB.FeatureType]

data CreateAuditorReportField
  = CreateAuditorReportFieldProfileId !DB.ProfileId
  -- is dry run
  | CreateAuditorReportDryRun !Bool
  -- ifpfs cid
  | CreateAuditorReportIpfsCid !DB.IpfsCid

data UpdateProfileRolesField
  = UpdateProfileRolesFieldProfileId DB.ProfileId
  | UpdateProfileRolesFieldRoles [DB.UserRole]

data GetRunTimeMetricsField
  = GetRunTimeMetricsFieldStart UTCTime
  | GetRunTimeMetricsFieldEnd UTCTime
  | GetRunTimeMetricsFieldRuns Int
  | GetRunTimeMetricsFieldMinimumTime NominalDiffTime

data GetSubscriptionsInIntervalField
  = GetSubscriptionsInIntervalFieldStart UTCTime
  | GetSubscriptionsInIntervalFieldEnd UTCTime
  | GetSubscriptionsInIntervalFieldSubscriptions Int

data GetAuditorReportMetricsField
  = GetAuditorReportMetricsFieldStart UTCTime
  | GetAuditorReportMetricsFieldEnd UTCTime
  | GetAuditorReportMetricsFieldReports Int

data GetProfileInvoicesField
  = GetProfileInvoicesFieldProfileId DB.ProfileId
  | GetProfileInvoicesFieldInvoicesNo Int

data GetAllInvoicesField
  = GetAllInvoicesFieldInvoicesNo Int
  | GetAllInvoicesFieldFrom (Maybe UTCTime)
  | GetAllInvoicesFieldTo (Maybe UTCTime)

data CancelInvoiceField
  = CancelInvoiceFieldSourceInvoiceId DB.InvoiceId
  | CancelInvoiceFieldByProfileId DB.ProfileId
  | CancelInvoiceFieldForProfileId DB.ProfileId
  | CancelInvoiceFieldResultingInvoiceId DB.InvoiceId

data CreateInvoiceField
  = CreateInvoiceFieldByProfileId DB.ProfileId
  | CreateInvoiceFieldForProfileId DB.ProfileId
  | CreateInvoiceFieldInvoiceId DB.InvoiceId
  | CreateInvoiceFieldAdaUsdPrice DB.AdaUsdPrice

data CreateSubscriptionInvoiceField
  = CreateSubscriptionInvoiceFieldByProfileId DB.ProfileId
  | CreateSubscriptionInvoiceFieldForProfileId DB.ProfileId
  | CreateSubscriptionInvoiceFieldInvoiceId DB.InvoiceId
  | CreateSubscriptionInvoiceFieldSubscriptionId DB.SubscriptionId

data ServerEventSelector f where
  InjectPersistenceSel :: forall f . !(DbSelector f) -> ServerEventSelector f
  Version :: ServerEventSelector Void
  WalletAddress :: ServerEventSelector Void
  CreateRun :: ServerEventSelector CreateRunField
  GetRun :: ServerEventSelector Void
  AbortRun :: ServerEventSelector RunIDV1
  GetRunDetails :: ServerEventSelector RunIDV1
  GetRunLogs :: ServerEventSelector RunIDV1
  GetProfileBalance :: ServerEventSelector DB.ProfileId
  GetCertification :: ServerEventSelector RunIDV1
  GetRepoInfo :: ServerEventSelector GetRepoInfoField
  CreateL1Certification :: ServerEventSelector CreateL1CertificationField
  Login :: ServerEventSelector WalletAddress
  ServerTimestamp :: ServerEventSelector Void
  GenerateGitHubToken :: ServerEventSelector GenerateGitHubTokenField
  GetGitHubClientId :: ServerEventSelector Void
  GetProfileSubscriptions :: ServerEventSelector DB.ProfileId
  Subscribe :: ServerEventSelector SubscribeField
  CancelProfilePendingSubscriptions :: ServerEventSelector CancelProfilePendingSubscriptionsField
  GetAllTiers :: ServerEventSelector Int
  GetActiveFeatures :: ServerEventSelector GetActiveFeaturesField
  GetAdaUsdPrice :: ServerEventSelector DB.AdaUsdPrice
  CreateAuditorReport :: ServerEventSelector CreateAuditorReportField
  GetProfileWalletAddress :: ServerEventSelector DB.ProfileId
  UpdateProfile :: ServerEventSelector DB.ProfileId
  UpdateProfileRoles :: ServerEventSelector UpdateProfileRolesField
  GetProfileRoles :: ServerEventSelector DB.ProfileId
  GetAllProfilesByRole :: ServerEventSelector DB.UserRole
  GetProfilesSummary :: ServerEventSelector DB.ProfileId
  GetRunTimeMetrics :: ServerEventSelector GetRunTimeMetricsField
  GetSubscriptionsStartingInInterval :: ServerEventSelector GetSubscriptionsInIntervalField
  GetSubscriptionsEndingInInterval :: ServerEventSelector GetSubscriptionsInIntervalField
  GetAuditorReportMetrics :: ServerEventSelector GetAuditorReportMetricsField
  GetProfileInvoices :: ServerEventSelector GetProfileInvoicesField
  GetAllInvoices :: ServerEventSelector GetAllInvoicesField
  CancelInvoice :: ServerEventSelector CancelInvoiceField
  CreateInvoice :: ServerEventSelector CreateInvoiceField
  CreateSubscriptionInvoice :: ServerEventSelector CreateSubscriptionInvoiceField
  InternalError :: forall a. ToJSON a => ServerEventSelector a


renderServerEventSelector :: RenderSelectorJSON ServerEventSelector
renderServerEventSelector Version = ("version", absurd)
renderServerEventSelector WalletAddress = ("wallet-address", absurd)
renderServerEventSelector GetRun = ("get-run", absurd)
renderServerEventSelector AbortRun = ("abort-run", renderRunIDV1)
renderServerEventSelector GetRunLogs = ("get-run-logs", renderRunIDV1)
renderServerEventSelector GetRunDetails = ("get-run-details", renderRunIDV1)
renderServerEventSelector GetProfileBalance = ("get-profile-balance", renderProfileId)
renderServerEventSelector GetCertification = ("get-certification", renderRunIDV1)
renderServerEventSelector Login = ("login", \address' -> ("user-address", toJSON address'))
renderServerEventSelector ServerTimestamp = ("server-timestamp", absurd)
renderServerEventSelector GenerateGitHubToken = ("generate-github-token", \
    (GenerateGitHubTokenError err) -> ("error", toJSON err)
  )
renderServerEventSelector GetGitHubClientId = ("get-github-client-id", absurd)
renderServerEventSelector GetProfileWalletAddress = ("get-profile-wallet-address", renderProfileId)

renderServerEventSelector GetProfileSubscriptions = ("get-profile-subscriptions", renderProfileId)
renderServerEventSelector Subscribe = ("subscribe", \case
    SubscribeFieldTierId tid -> ("tier-id", toJSON tid)
    SubscribeFieldProfileId pid -> ("profile-id", toJSON (show pid))
    SubscribeFieldSubscriptionId sid -> ("subscription-id", toJSON (show sid))
  )
renderServerEventSelector CancelProfilePendingSubscriptions = ("cancel-profile-pending-subscriptions", \case
    CancelProfilePendingSubscriptionsFieldProfileId pid -> ("profile-id", toJSON (show pid))
    CancelProfilePendingSubscriptionsFieldCount count' -> ("deleted-count", toJSON count')
  )
renderServerEventSelector CreateRun = ("create-run", \case
    CreateRunRef fr -> ("flake-reference", toJSON $ uriToString id fr.uri "")
    CreateRunID rid -> ("run-id", toJSON rid)
  )

renderServerEventSelector CreateL1Certification = ("start-certification", \case
    CreateL1CertificationRunID rid -> ("run-id", toJSON rid)
    CreateL1CertificationReportIpfsCid cid -> ("report-ipfs-cid", toJSON cid)
    CreateL1CertificationMetadataIpfsCid cid -> ("metadata-ipfs-cid", toJSON cid)
    CreateL1CertificationDryRun dryRun -> ("dry-run", toJSON dryRun)
  )

renderServerEventSelector GetRepoInfo = ("get-repo-info", \case
    GetRepoInfoOwner owner -> ("owner", toJSON owner)
    GetRepoInfoRepo repo -> ("repo", toJSON repo)
  )
renderServerEventSelector GetAllTiers = ("get-all-tiers", \count' -> ("count", toJSON count'))
renderServerEventSelector GetActiveFeatures = ("get-active-features", \case
    GetActiveFeaturesFieldProfileId pid -> ("profile-id", toJSON (show pid))
    GetActiveFeaturesFieldFeatures features -> ("features", toJSON features)
  )
renderServerEventSelector GetAdaUsdPrice = ("get-ada-usd-price", \price -> ("price", toJSON price))
renderServerEventSelector CreateAuditorReport = ("create-auditor-report", \case
    CreateAuditorReportFieldProfileId pid -> ("profile-id", toJSON (show pid))
    CreateAuditorReportDryRun isDryRun -> ("is-dry-run", toJSON isDryRun)
    CreateAuditorReportIpfsCid  cid -> ("cid", toJSON cid)
  )
renderServerEventSelector InternalError =
  ("internal-error", ("something-went-wrong",) . toJSON )
renderServerEventSelector UpdateProfile = ("update-profile", renderProfileId)
renderServerEventSelector UpdateProfileRoles = ("update-profile-roles", \case
    UpdateProfileRolesFieldProfileId pid -> ("profile-id", toJSON (show pid))
    UpdateProfileRolesFieldRoles roles -> ("roles", toJSON roles)
  )
renderServerEventSelector GetProfileRoles = ("get-profile-roles", renderProfileId)
renderServerEventSelector GetAllProfilesByRole = ("get-all-profiles-by-role",
  \role' -> ("role", toJSON role'))
renderServerEventSelector GetProfilesSummary = ("get-profiles-summary", renderProfileId)
renderServerEventSelector GetRunTimeMetrics = ("get-run-time-metrics", \case
  GetRunTimeMetricsFieldStart start -> ("start", toJSON start)
  GetRunTimeMetricsFieldEnd end -> ("end", toJSON end)
  GetRunTimeMetricsFieldRuns runs -> ("runs", toJSON runs)
  GetRunTimeMetricsFieldMinimumTime minimumTime -> ("minimum-time", toJSON minimumTime)
  )
renderServerEventSelector GetSubscriptionsStartingInInterval = ("get-subscriptions-starting-in-interval", \case
  GetSubscriptionsInIntervalFieldStart start -> ("start", toJSON start)
  GetSubscriptionsInIntervalFieldEnd end -> ("end", toJSON end)
  GetSubscriptionsInIntervalFieldSubscriptions subs -> ("subscriptions", toJSON subs)
  )
renderServerEventSelector GetSubscriptionsEndingInInterval = ("get-subscriptions-ending-in-interval", \case
  GetSubscriptionsInIntervalFieldStart start -> ("start", toJSON start)
  GetSubscriptionsInIntervalFieldEnd end -> ("end", toJSON end)
  GetSubscriptionsInIntervalFieldSubscriptions subs -> ("subscriptions", toJSON subs)
  )
renderServerEventSelector GetAuditorReportMetrics = ("get-auditor-report-metrics", \case
  GetAuditorReportMetricsFieldStart start -> ("start", toJSON start)
  GetAuditorReportMetricsFieldEnd end -> ("end", toJSON end)
  GetAuditorReportMetricsFieldReports reports -> ("reports", toJSON reports)
  )
renderServerEventSelector (InjectPersistenceSel s) = renderPersistenceSelector s
renderServerEventSelector GetProfileInvoices = ("get-profile-invoices", \case
  GetProfileInvoicesFieldProfileId pid -> ("profile-id", toJSON (show pid))
  GetProfileInvoicesFieldInvoicesNo len -> ("invoices", toJSON len)
  )
renderServerEventSelector GetAllInvoices = ("get-all-invoices", \case
  GetAllInvoicesFieldInvoicesNo len -> ("invoices", toJSON len)
  GetAllInvoicesFieldFrom start -> ("from", toJSON start)
  GetAllInvoicesFieldTo end -> ("to", toJSON end)
  )
renderServerEventSelector CancelInvoice = ("invoice-cancellation", \case
  CancelInvoiceFieldSourceInvoiceId invoiceId -> ("source-invoice-id", toJSON (show invoiceId))
  CancelInvoiceFieldByProfileId pid -> ("by-profile-id", toJSON (show pid))
  CancelInvoiceFieldForProfileId pid -> ("invoice-owner", toJSON (show pid))
  CancelInvoiceFieldResultingInvoiceId invoiceId -> ("cancellation-invoice-id", toJSON (show invoiceId))
  )
renderServerEventSelector CreateInvoice = ("invoice-creation", \case
  CreateInvoiceFieldInvoiceId invoiceId -> ("invoice-id", toJSON (show invoiceId))
  CreateInvoiceFieldByProfileId pid -> ("by-profile-id", toJSON (show pid))
  CreateInvoiceFieldForProfileId pid -> ("invoice-owner", toJSON (show pid))
  CreateInvoiceFieldAdaUsdPrice price -> ("ada-usd-price", toJSON price)
  )
renderServerEventSelector CreateSubscriptionInvoice = ("subscription-invoice-creation", \case
  CreateSubscriptionInvoiceFieldInvoiceId invoiceId -> ("invoice-id", toJSON (show invoiceId))
  CreateSubscriptionInvoiceFieldByProfileId pid -> ("by-profile-id", toJSON (show pid))
  CreateSubscriptionInvoiceFieldForProfileId pid -> ("invoice-owner", toJSON (show pid))
  CreateSubscriptionInvoiceFieldSubscriptionId subId -> ("subscription-id", toJSON (show subId))
  )

renderRunIDV1 :: RenderFieldJSON RunIDV1
renderRunIDV1 rid = ("run-id",toJSON rid)

renderProfileId :: RenderFieldJSON DB.ProfileId
renderProfileId pid = ("profile-id",toJSON (show pid))

type instance AuthServerData (AuthProtect "public-key") = (DB.ProfileId,ProfileWalletAddress)
type instance AuthServerData (AuthProtect "jwt-token") = (DB.ProfileId,ProfileWalletAddress)

toDbStatus :: RunStatusV1 -> DB.Status
toDbStatus (Finished _)= DB.Succeeded
toDbStatus (Incomplete (Preparing Failed)) = DB.Failed
toDbStatus (Incomplete (Building Failed)) = DB.Failed
toDbStatus (Incomplete (Certifying (CertifyingStatus Failed _ _))) = DB.Failed
toDbStatus _ = DB.Queued

toCertificationResult :: RunStatusV1 -> Maybe CertificationResult
toCertificationResult (Finished rep)= Just rep
toCertificationResult _ = Nothing

dbSync :: (MonadIO m,MonadMask m,MonadReader env m, HasDb env)
       => UUID -> RunStatusV1 -> m RunStatusV1
dbSync uuid' status = do
  now <- getNow
  let dbStatus = toDbStatus status
  void $ withDb $ case dbStatus of
    DB.Queued -> DB.syncRun uuid' now
    -- this will change to failed or succeeded only
    -- if the status is == Queued
    _ -> DB.updateFinishedRun uuid' (dbStatus == DB.Succeeded) now
  return status

getNow :: MonadIO m => m UTCTime
getNow = liftIO getCurrentTime

-- | this will create a new profile if there isn't any with the given address
ensureProfileFromBs :: forall m env.
                    ( MonadIO m
                    , MonadError ServerError m
                    , MonadMask m
                    , MonadReader env m
                    , HasDb env )
                    => ByteString
                    -> m (DB.ProfileId,ProfileWalletAddress)
ensureProfileFromBs bs = do
  case DB.mkPatternedText (decodeUtf8 bs) of
    Left err -> throwError $ err400 { errBody = BS.fromStrict $ BS.pack err }
    Right address' -> do
      retM <- ensureProfile address'
      case retM of
        Nothing -> throwError $ err500 { errBody = "Profile couldn't be created" }
        Just ret -> pure (ret,address')

ensureProfile :: forall m env.
              ( MonadIO m
              , MonadMask m
              , MonadReader env m
              , HasDb env )
              => DB.ProfileWalletAddress
              -> m (Maybe DB.ProfileId)
ensureProfile address' = do
      profileIdM <- withDb $ DB.getProfileId address'
      ensureProfile' profileIdM
  where
  ensureProfile'  (Just pid) = pure $ Just pid
  ensureProfile'  Nothing = withDb $ DB.upsertProfile
      (DB.Profile undefined address' Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
      Nothing
