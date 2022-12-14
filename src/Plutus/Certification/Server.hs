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
import Network.HTTP.Client      hiding (Proxy)
import Data.Text hiding (replicate, last)
import Data.ByteString.Lazy.Char8 qualified as LSB
import Paths_plutus_certification qualified as Package
import qualified Plutus.Certification.Web3StorageClient  as IPFS
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

newtype IpfsCID = IpfsCid Text
  deriving (ToJSON )
data CreateCertificationField
  = CreateCertificationRunID !RunIDV1
  | CreateCertificationIpfsCid !IpfsCID

data ServerEventSelector f where
  Version :: ServerEventSelector Void
  CreateRun :: ServerEventSelector CreateRunField
  GetRun :: ServerEventSelector Void
  AbortRun :: ServerEventSelector RunIDV1
  GetRunLogs :: ServerEventSelector RunIDV1
  GetCertification :: ServerEventSelector RunIDV1
  CreateCertification :: ServerEventSelector CreateCertificationField

renderServerEventSelector :: RenderSelectorJSON ServerEventSelector
renderServerEventSelector Version = ("version", absurd)
renderServerEventSelector CreateRun = ("create-run", \case
                                            CreateRunRef fr -> ("flake-reference", toJSON $ uriToString id fr.uri "")
                                            CreateRunID rid -> ("run-id", toJSON rid)
                                        )
renderServerEventSelector GetRun = ("get-run", absurd)
renderServerEventSelector AbortRun = ("abort-run", renderRunIDV1)
renderServerEventSelector GetRunLogs = ("get-run-logs", renderRunIDV1)
renderServerEventSelector GetCertification = ("get-certification", renderRunIDV1)
renderServerEventSelector CreateCertification = ("create-certification", \case
                                            CreateCertificationRunID rid -> ("run-id", toJSON $ rid)
                                            CreateCertificationIpfsCid cid -> ("cid", toJSON cid)
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
       -> EventBackend m r ServerEventSelector
       -> ServerT API m
server ServerCaps {..} eb = NamedAPI
  { version = withEvent eb Version . const . pure $ VersionV1 Package.version
  , versionHead = withEvent eb Version . const $ pure NoContent
  , createRun = \(profileId,_) fref@FlakeRef{..} -> withEvent eb CreateRun \ev -> do
      -- ensure the ref is in the right format before start the job
      (commitDate,commitHash) <- getCommitDateAndHash uri
      addField ev $ CreateRunRef fref
      res <- submitJob (setAncestor $ reference ev) fref
      addField ev $ CreateRunID res
      createDbRun fref profileId res commitDate commitHash
      pure res
  , getRun = \rid@RunID{..} -> withEvent eb GetRun \ev -> (
      runConduit
        $ getRuns (setAncestor $ reference ev) rid .| evalStateC Queued consumeRuns
      ) >>= dbSync uuid

  , abortRun = \(profileId,_) rid@RunID{..} -> withEvent eb AbortRun \ev -> do
      addField ev rid
      -- ensure is no already finished
      status <- runConduit $
        getRuns (setAncestor $ reference ev) rid .| evalStateC Queued consumeRuns
      unless (toDbStatus status == DB.Queued) $
        throwError err403 { errBody = "Run already finished"}
      requireRunIdOwner profileId uuid
      -- finally abort the run
      resp <- const NoContent <$> (abortRuns (setAncestor $ reference ev) rid)
      -- if abortion succeeded mark it in the db
      void (getNow >>= DB.withDb . DB.updateFinishedRun uuid False)

      pure resp
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
  , getCurrentProfile = \(profileId,_) ->
      let notFound = throwError $ err404 { errBody = "Profile not found" }
      in (DB.withDb $ DB.getProfile profileId) >>= maybe notFound pure

  -- TODO: add instrumentation
  , createCertification = \(profileId,_) rid@RunID{..} -> withEvent eb CreateCertification \ev -> do
    addField ev (CreateCertificationRunID rid)
    -- ensure runId belongs to the owner
    requireRunIdOwner profileId uuid

    -- ensure there is no certificate already created
    certificationM <- DB.withDb $ DB.getCertification uuid
    when (isJust certificationM) $ throwError err403 { errBody = "Certification already exists" }

    -- get the runId report
    certResultM <- toCertificationResult <$>
      (runConduit $ getRuns (setAncestor $ reference ev) rid .| evalStateC Queued consumeRuns)

    -- store the report into the ipfs
    (IPFS.UploadResponse ipfsCid _) <- maybe
      (throwError $ err403 { errBody = "Incompatible status for certification"})
      uploadToIpfs certResultM
    addField ev (CreateCertificationIpfsCid (IpfsCid ipfsCid))

    -- persist it into the db
    (DB.withDb . DB.createCertificate uuid ipfsCid =<< getNow)
      >>= maybe (throwError err500 { errBody = "Certification couldn't be persisted"} ) pure
  , getCertification = \rid@RunID{..} -> withEvent eb GetCertification \ev -> do
    addField ev rid
    (DB.withDb $ DB.getCertification uuid) >>=
      maybe (throwError err404 { errBody = "Certification not found"}) pure

  }
  where
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
    getCommitDateAndHash uri = do
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

