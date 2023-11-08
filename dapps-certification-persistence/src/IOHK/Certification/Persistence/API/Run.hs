{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}

module IOHK.Certification.Persistence.API.Run where

import Control.Monad
import Data.Maybe
import Database.Selda hiding (Set)
import IOHK.Certification.Persistence.Structure.Profile
import IOHK.Certification.Persistence.Structure.Run
import IOHK.Certification.Persistence.Structure.Certification
import IOHK.Certification.Persistence.Structure
import IOHK.Certification.Persistence.Pattern

import IOHK.Certification.Persistence.API.Profile

-- get all ready for certification runs
-- in ascending order
getRunsToCertify :: MonadSelda m => m [Run]
getRunsToCertify = query $ do
  run <- select runs
  restrict (run ! #runStatus .== literal ReadyForCertification)
  order (run ! #created) ascending
  pure run

getAllCertifiedRunsForAddress :: MonadSelda m => ProfileWalletAddress -> m [Run]
getAllCertifiedRunsForAddress address = query $ do
  -- get the profile id for the address
  profileId <- getProfileIdQ address
  run <- select runs
  restrict (run ! #profileId .== profileId)
  restrict (run ! #runStatus .== literal Certified)
  order (run ! #created) descending
  pure run

createRun :: MonadSelda m
          => UUID
          -> UTCTime
          -> Text
          -> UTCTime
          -> CommitHash
          -> CertificationPrice
          -> ID Profile
          -> m ()
createRun runId time repo commitDate commitHash certificationPrice pid = void $
  insert runs [Run runId time (Just time) time repo
    commitDate commitHash Queued pid certificationPrice Nothing]

getRunOwnerQ :: UUID -> Query t (Col t (ID Profile))
getRunOwnerQ runId = do
    p <- select runs
    restrict (p ! #runId .== literal runId )
    pure (p ! #profileId)

getRunOwner :: MonadSelda m => UUID -> m (Maybe (ID Profile))
getRunOwner = fmap listToMaybe . query . getRunOwnerQ

updateFinishedRun :: MonadSelda m => UUID -> Bool -> UTCTime -> m Int
updateFinishedRun runId succeeded time = do
  update runs
    (\run -> (run ! #runId .== literal runId) .&& (run ! #runStatus .== literal Queued))
    (`with`
      [ #runStatus := literal (if succeeded then Succeeded else Failed)
      , #syncedAt := literal time
      , #finishedAt := literal (Just time)
      ]
    )

syncRun :: MonadSelda m => UUID ->  UTCTime -> m Int
syncRun runId time= update runs
    (\run -> run ! #runId .== literal runId)
    (`with` [ #syncedAt := literal time ])

deleteRun :: MonadSelda m => UUID -> m Int
deleteRun runId = deleteFrom runs
  (\run -> (run ! #runId .== literal runId )
  .&& (run ! #runStatus ./= literal Certified)
  .&& (run ! #runStatus ./= literal ReadyForCertification))

markAsAborted :: MonadSelda m => UUID -> UTCTime -> m Int
markAsAborted runId time = update runs
    (\run -> (run ! #runId .== literal runId) .&& (run ! #runStatus .== literal Queued))
    (`with`
      [ #runStatus := literal Aborted
      , #syncedAt := literal time
      , #finishedAt := literal (Just time)
      ]
    )

markAllRunningAsAborted :: MonadSelda m => UTCTime -> m Int
markAllRunningAsAborted time = update runs
    (\run -> run ! #runStatus .== literal Queued)
    (`with`
      [ #runStatus := literal Aborted
      , #syncedAt := literal time
      , #finishedAt := literal (Just time)
      ]
    )

markAsReadyForCertification :: (MonadSelda m,MonadMask m)
                            => UUID
                            -> IpfsCid
                            -> UTCTime
                            -> m Int
markAsReadyForCertification runId IpfsCid{..}  time = update runs
  (\run -> (run ! #runId .== literal runId) .&& (run ! #runStatus .== literal Succeeded))
  (`with` [ #runStatus := literal ReadyForCertification
          , #syncedAt := literal time
          , #reportContentId := literal (Just ipfsCid)
          ])

createL1Certificate :: (MonadSelda m,MonadMask m)
                  => UUID
                  -> TxId
                  -> UTCTime
                  -> m (Maybe L1CertificationDTO)
createL1Certificate runId TxId{..} time = transaction $ do
  result <- query $ do
    run <- select runs
    restrict (run ! #runId .== literal runId)
    restrict ( run ! #runStatus .== literal ReadyForCertification)
    pure run
  case result of
    [_] -> do
      void $ update runs
        (\run -> run ! #runId .== literal runId)
        (`with` [ #runStatus := literal Certified
                , #syncedAt := literal time
                ])
      let cert = Certification def txId time
      certId <- insertWithPK certifications [cert]
      -- and now add a l1Certification
      let l1Cert = L1Certification runId certId
      _ <-  insert l1Certifications [l1Cert]
      pure $ Just (L1CertificationDTO l1Cert (#certId certId cert))
    _ -> pure Nothing

getL1CertificationQuery :: UUID -> Query t (Row t Certification :*: Row t L1Certification)
getL1CertificationQuery runID = do
    l1Cert <- select l1Certifications
    restrict (l1Cert ! #l1CertRunId .== literal runID )
    c <- innerJoin
      (\t -> t ! #certId .== l1Cert ! #l1CertId)
      (select certifications)
    pure (c :*: l1Cert)

getL1Certification :: MonadSelda m => UUID -> m (Maybe L1CertificationDTO)
getL1Certification pid = fmap (fmap toL1CertificationDTO . listToMaybe ) $ query $ getL1CertificationQuery pid

toL1CertificationDTO :: (Certification :*: L1Certification) -> L1CertificationDTO
toL1CertificationDTO  (cert :*: l1Cert) = L1CertificationDTO l1Cert cert

getRun :: MonadSelda m => UUID -> m (Maybe Run)
getRun rid = listToMaybe <$> query (do
  run <- select runs
  restrict (run ! #runId .== literal rid)
  pure run)

getRunStatus :: MonadSelda m => UUID -> m (Maybe Status)
getRunStatus rid = listToMaybe <$> query (do
  run <- select runs
  restrict (run ! #runId .== literal rid)
  pure (run ! #runStatus))

getRuns :: MonadSelda m => ID Profile -> Maybe UTCTime -> Maybe Int -> m [Run]
getRuns pid afterM topM = query $
  case topM of
    Just top -> limit 0 top select'
    Nothing  -> select'
  where
  select' = do
    run <- select runs
    restrict (run ! #profileId .== literal pid)
    case afterM of
      Just after -> restrict (run ! #created .< literal after)
      Nothing    -> pure ()
    order (run ! #created) descending
    pure run

-- TODO: because every run is synced with the real state
-- only when the api is called, we can't know for sure
-- if the run is finished or not.
-- For the moment, until we come up with a efficient monitoring solution,
-- we will consider the finishedAt or the syncedAt as the end of the run
getRunsInInterval :: MonadSelda m => UTCTime -> UTCTime -> m [Run]
getRunsInInterval start end = query $ do
  run <- select runs
  -- the convention for the filtering is T0 <= x < T1
  restrict (run ! #created .>= literal start)
  restrict ( run ! #finishedAt .< literal (Just end)
        .|| run ! #syncedAt .< literal end)
  pure run

