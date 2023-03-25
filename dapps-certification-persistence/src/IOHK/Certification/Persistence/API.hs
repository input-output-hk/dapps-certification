{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module IOHK.Certification.Persistence.API where

import           Control.Monad
import           Data.Maybe
import           Database.Selda
import           Database.Selda.SQLite
import           IOHK.Certification.Persistence.Structure

getTransactionIdQ:: Text -> Query t (Col t (ID Transaction))
getTransactionIdQ  externalAddress = do
  p <- select transactions
  restrict (p ! #wtxExternalId .== text externalAddress)
  pure (p ! #wtxId)

getTransactionId :: MonadSelda m => Text -> m (Maybe (ID Transaction))
getTransactionId = fmap listToMaybe . query . getTransactionIdQ

-- | inserts or updates a transaction
-- NOTE: if the transaction exists Nothing will be returned and entries
-- will not be updated
upsertTransaction :: (MonadSelda m,MonadMask m)
                  => Transaction
                  -> [TransactionEntry]
                  -> m (Maybe (ID Transaction))
upsertTransaction tx@Transaction{..} entries = do
  txIdM <- upsert transactions
     (\p -> p ! #wtxExternalId .== literal wtxExternalId)
     (`with`
      [ #wtxExternalId := literal wtxExternalId
      , #wtxAmount := literal wtxAmount
      , #wtxTime := literal wtxTime
      , #wtxDepth := literal wtxDepth
      , #wtxStatus := literal wtxStatus
      , #wtxMetadata := literal wtxMetadata
      ])
     [tx { wtxId = def :: ID Transaction}]
  -- if transaction was just inserted , insert entries as well
  -- otherwise we don't care to update entries
  let updateEntries txId' = flip fmap entries $
        \t -> t { txEntryTxId = txId', txEntryId = def :: ID TransactionEntry }
  forM_ txIdM (insert transactionEntries . updateEntries)
  pure txIdM

-- get all ready for certification runs
-- in ascending order
getRunsToCertify :: MonadSelda m => m [Run]
getRunsToCertify = query $ do
  run <- select runs
  restrict (run ! #runStatus .== literal ReadyForCertification)
  order (run ! #created) ascending
  pure run

getAllCertifiedRunsForAddress :: MonadSelda m => Text -> m [Run]
getAllCertifiedRunsForAddress address = query $ do
  -- get the profile id for the address
  profileId <- getProfileIdQ address
  run <- select runs
  restrict (run ! #profileId .== profileId)
  restrict (run ! #runStatus .== literal Certified)
  order (run ! #created) descending
  pure run

-- | get all available balance for a given address
-- | this is the sum of all the transactions minus the cost of all the certified runs
-- | if the address is not a profile owner Nothing will be returned
getProfileBalance :: MonadSelda m => Text -> m (Maybe Int)
getProfileBalance address = do
  profileIdM <- getProfileId address
  case profileIdM of
    Nothing -> pure Nothing
    Just _ -> do
      -- get all certified runs
      certifiedRuns <- getAllCertifiedRunsForAddress address
      -- get all the amounts coming from this address
      amountsFromAddress <- sum <$> getAllAmountsForAddress address
      -- sum all the costs of the certified runs
      let certifiedCosts = sum $ map certificationPrice certifiedRuns
          -- calculate the amount of credits available
          creditsAvailable = amountsFromAddress - certifiedCosts
      pure $ Just creditsAvailable

upsertProfile :: (MonadSelda m, MonadMask m) => Profile -> Maybe DApp -> m (Maybe (ID Profile))
upsertProfile profile@Profile{..} dappM = do
  void $ upsert profiles
     (\p -> p ! #ownerAddress .== text ownerAddress)
     (`with`
      [ #website     := fromTextMaybe website
      , #vendor      := fromTextMaybe vendor
      , #twitter     := fromTextMaybe twitter
      , #linkedin    := fromTextMaybe linkedin
      , #authors     := fromTextMaybe authors
      , #contacts    := fromTextMaybe contacts
      ])
     [#profileId (def :: ID Profile) profile]
  -- we query this because upsert returns id only when inserts
  profileIdM <- getProfileId ownerAddress
  forM_ profileIdM $ \pid -> case dappM of
    -- if there is no dapp we delete the dapp entry
    Nothing -> do
        void $ deleteFrom dapps (\dapp -> dapp ! #dappId .== literal pid)
    -- if there is a dapp we upsert it
    Just dapp@DApp{..} -> do
      void $ upsert dapps
          (\dapp' -> dapp' ! #dappId .== literal pid)
          (`with`
           [ #dappName := text dappName
           , #dappOwner := text dappOwner
           , #dappRepo := text dappRepo
           , #dappVersion := text dappVersion
           , #dappId := literal profileId
           , #dappGitHubToken := literal dappGitHubToken
           ]
          )
          [dapp { dappId = pid }]
  pure profileIdM
  where
  fromTextMaybe = maybe null_ (just . text)

getProfileByAddress :: MonadSelda m => Text -> m (Maybe Profile)
getProfileByAddress address = listToMaybe <$> query (do
    p <- select profiles
    restrict (p ! #ownerAddress .== text address)
    pure p
  )

getProfileQ :: ID Profile -> Query t (Row t Profile :*: Row t (Maybe DApp))
getProfileQ pid = do
  profile <- select profiles
  dapp <- leftJoin  (\dapp -> dapp ! #dappId .== literal pid) (select dapps)
  restrict (profile ! #profileId .== literal pid)
  pure (profile :*: dapp)

getProfileDAppQ :: ID Profile -> Query t (Row t DApp)
getProfileDAppQ pid = do
  dapp <- select dapps
  restrict (dapp ! #dappId .== literal pid)
  pure dapp

getProfile :: MonadSelda m => ID Profile -> m (Maybe ProfileDTO)
getProfile pid = fmap (fmap toProfileDTO . listToMaybe ) $ query $ getProfileQ pid

getProfileDApp :: MonadSelda m => ID Profile -> m (Maybe DApp)
getProfileDApp pid = fmap listToMaybe $ query $ getProfileDAppQ pid

toProfileDTO :: (Profile :*: Maybe DApp) -> ProfileDTO
toProfileDTO (profile :*: dapp) = ProfileDTO{..}

getProfileIdQ:: Text -> Query t (Col t (ID Profile))
getProfileIdQ  address = do
  p <- select profiles
  restrict (p ! #ownerAddress .== text address)
  pure (p ! #profileId)

getProfileId :: MonadSelda m => Text -> m (Maybe ProfileId)
getProfileId = fmap listToMaybe . query . getProfileIdQ

getProfileAddressQ :: ID Profile -> Query t (Col t Text)
getProfileAddressQ  pid = do
  p <- select profiles
  restrict (p ! #profileId .== literal pid)
  pure (p ! #ownerAddress)

getProfileAddress :: MonadSelda m => ID Profile -> m (Maybe Text)
getProfileAddress = fmap listToMaybe . query . getProfileAddressQ

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

getAllAmountsForAddress :: MonadSelda m => Text -> m [Int]
getAllAmountsForAddress address = query $ do
  input <- select transactionEntries
  restrict (input ! #txEntryAddress .== literal address .&& input ! #txEntryInput .== literal True)
  t <- innerJoin (\t -> (t ! #wtxId .== (input ! #txEntryTxId))
      .&& (t ! #wtxStatus .== literal InLedger)) (select transactions)
  pure (t ! #wtxAmount)

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

createCertificate :: (MonadSelda m,MonadMask m)
                  => UUID
                  -> TxId
                  -> UTCTime
                  -> m (Maybe Certification)
createCertificate runId TxId{..} time = transaction $ do
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
      let cert = Certification runId txId time
      _ <- insert certifications [cert]
      pure $ Just cert
    _ -> pure Nothing

getCertificationQuery :: UUID -> Query t (Row t Certification)
getCertificationQuery runID = do
    c <- select certifications
    restrict (c ! #certRunId .== literal runID )
    pure c

getCertification :: MonadSelda m => UUID -> m (Maybe Certification)
getCertification = fmap listToMaybe . query . getCertificationQuery

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

--TODO: replace this with a proper configuration
withDb :: (MonadIO m, MonadMask m) => SeldaT SQLite m a -> m a
withDb = withSQLite "certification.sqlite"
