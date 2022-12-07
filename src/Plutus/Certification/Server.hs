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
import IOHK.Certification.Interface
import Servant.Server.Experimental.Auth
import Plutus.Certification.API as API
import Plutus.Certification.GithubClient 
import qualified IOHK.Certification.Persistence as DB
import Control.Monad.Except
import Data.Time.LocalTime
import Data.Maybe
import Data.Time
import Data.Text hiding (replicate, last)
import Data.ByteString.Lazy.Char8 qualified as LSB
import Paths_plutus_certification qualified as Package


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

data ServerEventSelector f where
  Version :: ServerEventSelector Void
  CreateRun :: ServerEventSelector CreateRunField
  GetRun :: ServerEventSelector Void
  AbortRun :: ServerEventSelector RunIDV1
  GetRunLogs :: ServerEventSelector RunIDV1

renderServerEventSelector :: RenderSelectorJSON ServerEventSelector
renderServerEventSelector Version = ("version", absurd)
renderServerEventSelector CreateRun = ("create-run", \case
                                            CreateRunRef fr -> ("flake-reference", toJSON $ uriToString id fr.uri "")
                                            CreateRunID rid -> ("run-id", toJSON rid)
                                        )
renderServerEventSelector GetRun = ("get-run", absurd)
renderServerEventSelector AbortRun = ("abort-run", \rid -> ("run-id",toJSON rid))
renderServerEventSelector GetRunLogs = ("get-run-logs", \rid -> ("run-id",toJSON rid))

newtype UserAddress = UserAddress { unUserAddress :: Text}

type instance AuthServerData (AuthProtect "public-key") = (DB.ProfileId,UserAddress)

toDbStatus :: RunStatusV1 -> DB.Status
toDbStatus (Finished _)= DB.Succeeded
toDbStatus (Incomplete (Preparing Failed)) = DB.Failed
toDbStatus (Incomplete (Building Failed)) = DB.Failed
toDbStatus (Incomplete (Certifying (CertifyingStatus Failed _ _))) = DB.Failed
toDbStatus _ = DB.Queued

fromFlakeToCommitDate :: FlakeRefV1 -> IO UTCTime
fromFlakeToCommitDate = undefined
  where

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
      commitDate <- getCommitDate uri
      addField ev $ CreateRunRef fref
      res <- submitJob (setAncestor $ reference ev) fref
      addField ev $ CreateRunID res
      createDbRun fref profileId res commitDate
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
      -- ensure is the owner of the run
      isOwner <- (== (Just profileId)) <$> (DB.withDb $ DB.getRunOwner uuid)
      unless (isOwner) $ throwError err403
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
      DB.withDb $ do
        _ <- DB.upsertProfile (DB.Profile{..})
        -- it's safe to call partial function fromJust
        fromJust <$> DB.getProfile profileId
  , getCurrentProfile = \(profileId,_) ->
      let notFound = throwError $ err404 { errBody = "Profile not found" }
      in (DB.withDb $ DB.getProfile profileId) >>= maybe notFound pure
  }
  where
    getCommitDate uri = do
      (owner,repo,path') <- extractUriSegments uri
      commitInfoE <- liftIO $ getCommitInfo owner repo path'
      case commitInfoE of
           Left e -> throwError err400 { errBody = LSB.pack $ show e}
           Right (Commit _ (CommitDetails _ _ (Author _ _ time))) -> pure time
    extractUriSegments uri = case fmap pack $ pathSegments uri of
        owner:repo:path':_ -> pure (owner,repo,path')
        _ -> throwError err400 { errBody = "Wrong flake-ref details"}
    getNow = liftIO $ getCurrentTime
    createDbRun FlakeRef{..} profileId res commitDate = do
      now <- getNow
      let uriTxt = pack $ uriToString id uri ""
      DB.withDb $ DB.createRun (uuid res) now uriTxt commitDate profileId
    dbSync uuid status = do
      now <- getNow
      let dbStatus = toDbStatus status
      void $ DB.withDb $ case dbStatus of
        DB.Queued -> DB.syncRun uuid now
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

