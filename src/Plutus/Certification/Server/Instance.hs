{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Monad.State.Strict
import Observe.Event
import Observe.Event.BackendModification
import Network.URI
import Plutus.Certification.API as API
import Plutus.Certification.GithubClient
import Control.Monad.Except
import Data.Time.LocalTime
import Data.Maybe
import Data.Aeson
import Network.HTTP.Client      hiding (Proxy,parseUrl)

import Network.HTTP.Types

import Data.Text as Text hiding (elem,replicate, last)
import Data.Text.Encoding
import Plutus.Certification.WalletClient (WalletArgs(walletCertificationPrice))
import IOHK.Certification.Interface hiding (Status)
import Plutus.Certification.Server.Internal

import Data.ByteString.Lazy.Char8 qualified as LSB
import Paths_plutus_certification qualified as Package
import qualified Plutus.Certification.WalletClient as Wallet
import qualified IOHK.Certification.Persistence as DB
import qualified Plutus.Certification.Web3StorageClient  as IPFS

hoistServerCaps :: (Monad m) => (forall x . m x -> n x) -> ServerCaps m r -> ServerCaps n r
hoistServerCaps nt (ServerCaps {..}) = ServerCaps
  { submitJob = \mods -> nt . submitJob mods
  , getRuns = \mods -> transPipe nt . getRuns mods
  , abortRuns = \mods -> nt . abortRuns mods
  , getLogs = \mods act -> transPipe nt . getLogs mods act
  }

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
  , getRun = \rid@RunID{..} -> withEvent eb GetRun \ev -> runConduit
      ( getRuns (setAncestor $ reference ev) rid .| evalStateC Queued consumeRuns
      ) >>= dbSync uuid
  , getRunDetails = \rid@RunID{..} -> withEvent eb GetRunDetails \ev -> do
      addField ev rid
      -- get the run details from the db
      DB.withDb ( DB.getRun uuid )
        >>= maybeToServerError err404 "Run not found"
  , abortRun = \(profileId,_) rid@RunID{..} deleteRun -> withEvent eb AbortRun \ev -> do
      addField ev rid
      requireRunIdOwner profileId uuid
      -- abort the run if is still running
      status <- runConduit $
        getRuns (setAncestor $ reference ev) rid .| evalStateC Queued consumeRuns
      when (toDbStatus status == DB.Queued) $ do
        -- finally abort the run
        abortRuns (setAncestor $ reference ev) rid
        -- if abortion succeeded mark it in the db
        void (getNow >>= DB.withDb . DB.updateFinishedRun uuid False)

      when (deleteRun == Just True) $
        -- delete the run from the db
        void (DB.withDb $ DB.deleteRun uuid)
      pure NoContent
  , getProfileBalance = \(profileId,UserAddress{..}) -> withEvent eb GetProfileBalance \ev -> do
      addField ev profileId
      fromMaybe 0 <$> DB.withDb (DB.getProfileBalance unUserAddress)
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
  , createCertification = \(profileId,_) rid@RunID{..} -> withEvent eb StartCertification \ev -> do
    addField ev (StartCertificationRunID rid)
    -- ensure runId belongs to the owner
    requireRunIdOwner profileId uuid

    -- get the runId report
    status <- runConduit (getRuns (setAncestor $ reference ev) rid .| evalStateC Queued consumeRuns)

    -- sync the run with the db and return the db-run information
    DB.Run{runStatus} <- getRunAndSync rid status

    let certResultM = toCertificationResult status
    (IPFS.UploadResponse ipfsCid _) <- maybe
      (throwError $ err403 { errBody = "Incompatible status for certification"})
      uploadToIpfs certResultM
    addField ev (StartCertificationIpfsCid ipfsCid)

    -- ensure there is no certificate already created
    when (runStatus `elem` [DB.ReadyForCertification, DB.Certified]) $
      throwError err403 { errBody = "Certification already started" }

    -- ensure the run is finished
    unless (runStatus == DB.Succeeded) $
      throwError err403 { errBody = "Transaction status not fit for certification" }

    -- mark the run as ready for certification
    getNow
      >>= DB.withDb . DB.markAsReadyForCertification uuid ipfsCid
      >> pure NoContent -- yep, we don't need to return anything

  , getCertification = \rid@RunID{..} -> withEvent eb GetCertification \ev -> do
    addField ev rid
    DB.withDb (DB.getCertification uuid)
      >>= maybeToServerError err404 "Certification not found"
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
        Right result -> pure result
    getRunAndSync RunID{..} status = do
      run <- DB.withDb (DB.getRun uuid)
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

      let uri = "github:" <> encodeUtf8 dappOwner <> "/" <> encodeUtf8 dappRepo <> "/" <> encodeUtf8 commitOrBranch
      eitherToServerError
        err400 LSB.pack
        (mimeUnrender (Proxy :: Proxy PlainText) (LSB.fromStrict uri))

    forbidden str = throwError $ err403 { errBody = str}

    getProfileDApp profileId = DB.withDb (DB.getProfileDApp profileId)
        >>= withDappNotAvailableMsg

    withDappNotAvailableMsg = maybeToServerError err403 "DApp profile data not available"

    getProfileDTO profileId = DB.withDb (DB.getProfile profileId)
        >>= maybeToServerError err404 "Profile not found"

    requireRunIdOwner profileId uuid = do
      -- ensure is the owner of the run
      isOwner <- (== Just profileId) <$> DB.withDb (DB.getRunOwner uuid)
      unless isOwner $ throwError err403

    getCommitDateAndHash FlakeRef{..} = do
      (owner,repo,path') <- extractUriSegments uri
      commitInfoE <- liftIO $ getCommitInfo owner repo path'
      case commitInfoE of
           Left e -> throwError err400 { errBody = LSB.pack $ show e}
           Right (Commit hash (CommitDetails _ _ (Author _ _ time))) -> pure (time,hash)

    extractUriSegments uri = case pack <$> pathSegments uri of
        owner:repo:path':_ -> pure (owner,repo,path')
        _ -> throwError err400 { errBody = "Wrong flake-ref details"}

    createDbRun FlakeRef{..} profileId res commitDate commitHash= do
      now <- getNow
      let uriTxt = pack $ uriToString id uri ""
      DB.withDb $ DB.createRun (uuid res) now uriTxt commitDate
        commitHash (wargs.walletCertificationPrice) profileId
