{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Plutus.Certification.Server where

import Conduit
import Control.Monad.Catch
import Data.Aeson
import Data.Void
import Servant
import Control.Monad.State.Strict
import Observe.Event
import Observe.Event.BackendModification
import Observe.Event.Render.JSON
import Network.URI
import IOHK.Certification.Interface hiding (Status)
import Servant.Server.Experimental.Auth
import Plutus.Certification.API as API
import Plutus.Certification.GithubClient
import Control.Monad.Except
import Data.Time.LocalTime
import Data.Maybe
import Data.Time
import Network.HTTP.Types
import Network.HTTP.Client      hiding (Proxy,parseUrl)
import Servant.Client
import Data.Text as Text hiding (replicate, last)
import Data.Text.Encoding
import Data.ByteString.Lazy.Char8 qualified as LSB
import Paths_plutus_certification qualified as Package
import qualified Plutus.Certification.Web3StorageClient  as IPFS
import qualified Plutus.Certification.WalletClient as Wallet
import qualified IOHK.Certification.Persistence as DB

-- | Capabilities needed to run a server for 'API'
data ServerCaps m r = ServerCaps
  { -- | Submit a new certification job
    submitJob :: !(EventBackendModifiers r r -> FlakeRefV1 -> m RunIDV1)
  , -- | Get the status of all runs associated with a job
    getRuns :: !(EventBackendModifiers r r -> RunIDV1 -> ConduitT () RunStatusV1 m ())
  , -- | Delete all runs associated with a job
    abortRuns :: !(EventBackendModifiers r r -> RunIDV1 -> m ())
  , -- | Get the logs for all runs associated with a job
    getLogs :: !(EventBackendModifiers r r -> Maybe KnownActionType -> RunIDV1 -> ConduitT () RunLog m ())
  }

hoistServerCaps :: (Monad m) => (forall x . m x -> n x) -> ServerCaps m r -> ServerCaps n r
hoistServerCaps nt (ServerCaps {..}) = ServerCaps
  { submitJob = \mods -> nt . submitJob mods
  , getRuns = \mods -> transPipe nt . getRuns mods
  , abortRuns = \mods -> nt . abortRuns mods
  , getLogs = \mods act -> transPipe nt . getLogs mods act
  }

data CreateRunField
  = CreateRunRef !FlakeRefV1
  | CreateRunID !RunIDV1

data CreateCertificationField
  = CreateCertificationRunID !RunIDV1
  | CreateCertificationIpfsCid !DB.IpfsCid
  | CreateCertificationTxResponse !Wallet.TxResponse

data ServerEventSelector f where
  Version :: ServerEventSelector Void
  WalletAddress :: ServerEventSelector Void
  CreateRun :: ServerEventSelector CreateRunField
  GetRun :: ServerEventSelector Void
  AbortRun :: ServerEventSelector RunIDV1
  GetRunLogs :: ServerEventSelector RunIDV1
  GetCertification :: ServerEventSelector RunIDV1
  CreateCertification :: ServerEventSelector CreateCertificationField

renderServerEventSelector :: RenderSelectorJSON ServerEventSelector
renderServerEventSelector Version = ("version", absurd)
renderServerEventSelector WalletAddress = ("wallet-address", absurd)
renderServerEventSelector GetRun = ("get-run", absurd)
renderServerEventSelector AbortRun = ("abort-run", renderRunIDV1)
renderServerEventSelector GetRunLogs = ("get-run-logs", renderRunIDV1)
renderServerEventSelector GetCertification = ("get-certification", renderRunIDV1)

renderServerEventSelector CreateRun = ("create-run", \case
    CreateRunRef fr -> ("flake-reference", toJSON $ uriToString id fr.uri "")
    CreateRunID rid -> ("run-id", toJSON rid)
  )

renderServerEventSelector CreateCertification = ("create-certification", \case
    CreateCertificationRunID rid -> ("run-id", toJSON $ rid)
    CreateCertificationIpfsCid cid -> ("cid", toJSON cid)
    CreateCertificationTxResponse txResp -> ("tx-resp",toJSON txResp)
  )

renderRunIDV1 :: RenderFieldJSON RunIDV1
renderRunIDV1 = \rid -> ("run-id",toJSON rid)

newtype UserAddress = UserAddress { unUserAddress :: Text}

type instance AuthServerData (AuthProtect "public-key") = (DB.ProfileId,UserAddress)

toDbStatus :: RunStatusV1 -> DB.Status
toDbStatus (Finished _)= DB.Succeeded
toDbStatus (Incomplete (Preparing Failed)) = DB.Failed
toDbStatus (Incomplete (Building Failed)) = DB.Failed
toDbStatus (Incomplete (Certifying (CertifyingStatus Failed _ _))) = DB.Failed
toDbStatus _ = DB.Queued

toCertificationResult :: RunStatusV1 -> Maybe CertificationResult
toCertificationResult (Finished rep)= Just rep
toCertificationResult _ = Nothing

--NOTE
getLastPart :: URI -> String
getLastPart uri = last $ pathSegments uri

-- | An implementation of 'API'
server :: (MonadMask m,MonadIO m, MonadError ServerError m)
       => ServerCaps m r
       -> Wallet.WalletArgs
       -> EventBackend m r ServerEventSelector
       -> ServerT API m
server ServerCaps {..} wargs eb = NamedAPI
  { version = withEvent eb Version . const . pure $ VersionV1 Package.version
  , versionHead = withEvent eb Version . const $ pure NoContent
  , walletAddress = withEvent eb WalletAddress . const $ pure wargs.walletAddress
  , createRun = \(profileId,_) commitOrBranch -> withEvent eb CreateRun \ev -> do
      fref <- getFlakeRef profileId commitOrBranch
      -- ensure the ref is in the right format before start the job
      (commitDate,commitHash) <- getCommitDateAndHash fref
      addField ev $ CreateRunRef fref
      res <- submitJob (setAncestor $ reference ev) fref
      addField ev $ CreateRunID res
      createDbRun fref profileId res commitDate commitHash
      pure res
  , getRun = \rid@RunID{..} -> withEvent eb GetRun \ev -> (
      runConduit
        $ getRuns (setAncestor $ reference ev) rid .| evalStateC Queued consumeRuns
      ) >>= dbSync uuid

  , abortRun = \(profileId,_) rid@RunID{..} deleteRun -> withEvent eb AbortRun \ev -> do
      addField ev rid
      requireRunIdOwner profileId uuid
      -- abort the run if is still running
      status <- runConduit $
        getRuns (setAncestor $ reference ev) rid .| evalStateC Queued consumeRuns
      when (toDbStatus status == DB.Queued) $ do
        -- finally abort the run
        (abortRuns (setAncestor $ reference ev) rid)
        -- if abortion succeeded mark it in the db
        void (getNow >>= DB.withDb . DB.updateFinishedRun uuid False)

      when (deleteRun == Just True) $
        -- detele the run from the db
        void (DB.withDb $ DB.deleteRun uuid)
      pure NoContent
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
  , getRuns = \(profileId,_) afterM countM -> do
      DB.withDb $ DB.getRuns profileId afterM countM
  , updateCurrentProfile = \(profileId,UserAddress ownerAddress) ProfileBody{..} -> do
      let dappId = profileId
      let dappM = fmap (\DAppBody{..} -> DB.DApp{..}) dapp
      DB.withDb $ do
        _ <- DB.upsertProfile (DB.Profile{..}) dappM
        -- it's safe to call partial function fromJust

        fromJust <$> DB.getProfile profileId
  , getCurrentProfile = \(profileId,_) -> getProfileDTO profileId

  -- TODO: Add instrumentation
  -- TODO: There might be an issue if more than one certification is started at
  -- the same time for the same Run:
  -- Multiple transactions are going to be broadcasted at the same time and
  -- therefore we are going to pay multiple fees.
  -- We have to somehow create a lock mechanism for every certification per run
  , createCertification = \(profileId,_) rid@RunID{..} -> withEvent eb CreateCertification \ev -> do
    addField ev (CreateCertificationRunID rid)
    -- ensure runId belongs to the owner
    requireRunIdOwner profileId uuid

    -- ensure there is no certificate already created
    certificationM <- DB.withDb $ DB.getCertification uuid
    when (isJust certificationM) $ throwError err403 { errBody = "Certification already exists" }

    -- getting required profile information before further processing
    (DB.Profile{..},dapp@DB.DApp{..}) <- getProfileAndDApp profileId

    -- get the runId report
    status <- (runConduit $ getRuns (setAncestor $ reference ev) rid .| evalStateC Queued consumeRuns)

    -- sync the run with the db and return the db-run information
    DB.Run{..} <- getRunAndSync rid status

    -- store the report into the ipfs
    let certResultM = toCertificationResult status
    (IPFS.UploadResponse ipfsCid _) <- maybe
      (throwError $ err403 { errBody = "Incompatible status for certification"})
      uploadToIpfs certResultM
    addField ev (CreateCertificationIpfsCid ipfsCid)

    -- create the certification object
    websiteUrl <- parseUrl website
    FlakeRef{..} <- createFlakeRef dapp (CommitOrBranch commitHash)
    let certificate = Wallet.CertificationMetadata uuid ipfsCid dappName websiteUrl twitter uri dappVersion

    -- broadcast the certification
    tx@Wallet.TxResponse{..} <- (Wallet.broadcastTransaction wargs certificate)
      >>= eitherToServerError err500 (LSB.pack . show)
    addField ev (CreateCertificationTxResponse tx)

    -- persist it into the db
    (DB.withDb . DB.createCertificate uuid ipfsCid txRespId =<< getNow)
      >>= maybeToServerError err500 "Certification couldn't be persisted"

  , getCertification = \rid@RunID{..} -> withEvent eb GetCertification \ev -> do
    addField ev rid
    (DB.withDb $ DB.getCertification uuid)
      >>= maybeToServerError err404 "Certification not found"

  }
  where
    getRunAndSync RunID{..} status = do
      run <- (DB.withDb $ DB.getRun uuid)
        >>= maybeToServerError err404 "No Run"
      _ <- dbSync uuid status
      pure run

    getFlakeRef profileId commitOrBranch =
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

      let uri = ("github:" <> encodeUtf8 dappOwner <> "/" <> encodeUtf8 dappRepo <> "/" <> encodeUtf8 commitOrBranch )
      eitherToServerError
        err400 LSB.pack
        (mimeUnrender (Proxy :: Proxy PlainText) (LSB.fromStrict uri))

    parseUrl website = eitherToServerError err403 (LSB.pack . show)
        (maybe (Right Nothing) (fmap Just . parseBaseUrl . unpack ) website)

    forbidden str = throwError $ err403 { errBody = str}

    getProfileDApp profileId = (DB.withDb $ DB.getProfileDApp profileId)
        >>= withDappNotAvailableMsg

    withDappNotAvailableMsg = maybeToServerError err403 "DApp profile data not available"

    getProfileAndDApp profileId = do
      DB.ProfileDTO{..} <- getProfileDTO profileId
      dapp' <- withDappNotAvailableMsg dapp
      pure (profile,dapp')

    getProfileDTO profileId = (DB.withDb $ DB.getProfile profileId)
        >>= maybeToServerError err404 "Profile not found"

    uploadToIpfs :: (Monad m, MonadIO m, MonadError ServerError m) => CertificationResult -> m IPFS.UploadResponse
    uploadToIpfs certResultM = do
      resp <- IPFS.uploadReportToIpfs IPFS.apiKey (LSB.toStrict $ encode certResultM)
      case resp of
        Left (IPFS.DecodeFailure _ err) -> throwError err500 { errBody = encode err }
        Left (IPFS.HttpError resp') ->
          let (Status code msg) = responseStatus resp'
              err = ServerError code "IPFS gateway error" (LSB.fromStrict msg) []
          in throwError err
        Right result -> pure $ result

    requireRunIdOwner profileId uuid = do
      -- ensure is the owner of the run
      isOwner <- (== (Just profileId)) <$> (DB.withDb $ DB.getRunOwner uuid)
      unless (isOwner) $ throwError err403

    getCommitDateAndHash FlakeRef{..} = do
      (owner,repo,path') <- extractUriSegments uri
      commitInfoE <- liftIO $ getCommitInfo owner repo path'
      case commitInfoE of
           Left e -> throwError err400 { errBody = LSB.pack $ show e}
           Right (Commit hash (CommitDetails _ _ (Author _ _ time))) -> pure (time,hash)

    extractUriSegments uri = case fmap pack $ pathSegments uri of
        owner:repo:path':_ -> pure (owner,repo,path')
        _ -> throwError err400 { errBody = "Wrong flake-ref details"}

    getNow = liftIO $ getCurrentTime

    createDbRun FlakeRef{..} profileId res commitDate commitHash= do
      now <- getNow
      let uriTxt = pack $ uriToString id uri ""
      DB.withDb $ DB.createRun (uuid res) now uriTxt commitDate commitHash profileId

    dbSync uuid status = do
      now <- getNow
      let dbStatus = toDbStatus status
      void $ DB.withDb $ case dbStatus of
        DB.Queued -> DB.syncRun uuid now
        DB.Certified -> pure 0 -- do nothing, it is already certified
        _ -> DB.updateFinishedRun uuid (dbStatus == DB.Succeeded) now
      return status

    consumeRuns = await >>= \case
      Nothing -> Incomplete <$> get
      Just (Incomplete s) -> do
        modify \s' -> case (s, s') of
          (_, Queued) -> s
          (Queued, _) -> s'

          (Preparing st, Preparing st') -> case compare st st' of
            LT -> s'
            _ -> s
          (_, Preparing _) -> s
          (Preparing _, _) -> s'

          (Building st, Building st') -> case compare st st' of
            LT -> s'
            _ -> s
          (_, Building _) -> s
          (Building _, _) -> s'

          (Certifying (CertifyingStatus st mp _), Certifying (CertifyingStatus st' mp' _)) -> case compare st st' of
            LT -> s'
            GT -> s
            EQ -> case (mp, mp') of
              (_, Nothing) -> s
              (Nothing, _) -> s'
              (Just p, Just p') -> case compare p.progressIndex p'.progressIndex of
                LT -> s'
                _ -> s
        consumeRuns
      Just s -> pure s

