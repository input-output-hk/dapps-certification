{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedRecordDot #-}

module IOHK.Certification.Persistence.API where

import Control.Monad
import Data.Maybe
import Data.List (nub)
import Database.Selda
import Database.Selda.SQLite
import IOHK.Certification.Persistence.Structure.Profile
import IOHK.Certification.Persistence.Structure.Subscription as Subscription
import IOHK.Certification.Persistence.Structure
import Data.Time.Clock
import Data.Int

import Data.Fixed

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

getPendingSubscriptions :: MonadSelda m => m [Subscription]
getPendingSubscriptions = query $ do
  sub <- select subscriptions
  restrict (sub ! #subscriptionStatus .== literal PendingSubscription)
  pure sub

activateAllPendingSubscriptions :: MonadSelda m => m [ID Subscription]
activateAllPendingSubscriptions = do
  updates <- mapM activateSubscription =<< getPendingSubscriptions
  pure $ catMaybes updates

activateSubscription :: MonadSelda m => Subscription -> m (Maybe (ID Subscription))
activateSubscription sub = do
  profile <- getProfile (sub.subscriptionProfileId)
  case profile of
    Just p -> do
      balance <- fromMaybe 0 <$> getProfileBalance (p.profile.ownerAddress)
      if balance >= sub.subscriptionPrice
      then do
        update_ subscriptions
          (\s -> s ! #subscriptionId .== literal (sub.subscriptionId))
          (`with` [ #subscriptionStatus := literal ActiveSubscription])
        pure $ Just (sub.subscriptionId)
      else
        pure Nothing
    Nothing -> pure Nothing


getAllCertifiedRunsForAddress :: MonadSelda m => Text -> m [Run]
getAllCertifiedRunsForAddress address = query $ do
  -- get the profile id for the address
  profileId <- getProfileIdQ address
  run <- select runs
  restrict (run ! #profileId .== profileId)
  restrict (run ! #runStatus .== literal Certified)
  order (run ! #created) descending
  pure run

getAllPaidSubscriptions :: MonadSelda m => ID Profile -> m [Subscription]
getAllPaidSubscriptions profileId = query $ do
  sub <- select subscriptions
  restrict $ (sub ! #subscriptionStatus .== literal InactiveSubscription
           .|| sub ! #subscriptionStatus .== literal ActiveSubscription)
           .&& sub ! #subscriptionProfileId .== literal profileId
  pure sub

-- | get all available balance for a given address
-- | this is the sum of all the transactions minus the cost of all the certified runs
-- | if the address is not a profile owner Nothing will be returned
getProfileBalance :: MonadSelda m => Text -> m (Maybe Int64)
getProfileBalance address = do
  profileIdM <- getProfileId address
  case profileIdM of
    Nothing -> pure Nothing
    Just pid -> do
      -- get all certified runs
      certifiedRuns <- getAllCertifiedRunsForAddress address
      -- get all paid subscriptions
      paidSubscriptions <- getAllPaidSubscriptions pid
      -- get all the amounts coming from this address
      amountsFromAddress <- sum <$> getAllAmountsForAddress address
      -- sum all the costs of the certified runs
      let certifiedCosts = sum $ map certificationPrice certifiedRuns
          -- calculate the amount of credits available for subscriptions
          subscriptionCredits = sum $ map subscriptionPrice paidSubscriptions
          -- calculate the amount of credits available
          creditsAvailable = amountsFromAddress - certifiedCosts - subscriptionCredits
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

getAllAmountsForAddress :: MonadSelda m => Text -> m [Int64]
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

addInitialData :: MonadSelda m => m ()
addInitialData = void $ do
  let devFeatures = [ Feature L1Run "allows running testing campaign for a DApp" ]
      auditFeatures = [ Feature L2UploadReport "allows an auditor to upload a report" ]
  insert_ features (devFeatures <> auditFeatures)
  -- TODO: price should be configurable
  let usdPrice = 2
  -- TODO: subscription duration should be configurable
  let developer =
        [ Tier def "Standard" "Minimal Features to get L1 certificate"
          "This tier is perfect for developers who are just starting out with securing their software.\
          \ With this tier, you will have access to our basic features, which will help you get a L1 certificate."
          Developer usdPrice 365 True ]
      auditor  =
        [Tier def "Standard" "All Features and customizations + upload of a report on chain"
         "This tier is perfect for auditors who want full access to all of our features.\
         \ With this tier, you will have access to our full suite of tools, which will help you ensure that your clients' software is fully compliant with industry standards."
        Auditor usdPrice 365 True]
  developerId <- insertWithPK tiers developer
  auditorId <- insertWithPK tiers auditor
  insert_ tierFeatures  [TierFeature def developerId (featureId f) | f <- devFeatures]
  insert_ tierFeatures  [TierFeature def auditorId (featureId f) | f <- devFeatures <> auditFeatures]

type AdaUsdPrice = Micro
-- | Create a subscription for a profile based on a tier.
-- creates a pending subscription and disables all other subscriptions for this profile
createSubscription :: MonadSelda m => UTCTime -> ID Profile -> ID Tier -> AdaUsdPrice  -> m (Maybe SubscriptionDTO)
createSubscription startDate pid tid adaUsdPrice = do
  tierM <- listToMaybe <$> query (do
    tier <- select tiers
    restrict (tier ! #tierId .== literal tid)
    pure tier)
  -- if tier exists create a subscription
  forM tierM $ \tier -> do
    let endDate = addDaysToUTCTime (tierDuration tier) startDate
    -- delete all previous pending subscriptions for this profile
    _ <- cancelPendingSubscription pid

    let subscriptionPrice = usdToLovelace tier.tierUsdPrice adaUsdPrice
        subscriptionToInsert = Subscription def pid tid (tier.tierName) (tier.tierType)
          subscriptionPrice startDate endDate PendingSubscription
    -- create a new subscription
    subscriptionId <- insertWithPK subscriptions [subscriptionToInsert]
    let subscription = subscriptionToInsert { subscriptionId = subscriptionId}

    -- and disable all other subscriptions
    update_ subscriptions
      (\s -> s ! #subscriptionProfileId .== literal pid .&& s ! #subscriptionId ./= literal subscriptionId)
      (`with` [ #subscriptionStatus := literal InactiveSubscription ])
    toSubscriptionDTO subscription
  where
  addDaysToUTCTime days = addUTCTime (nominalDay * fromIntegral days)

-- usd to lovelace
usdToLovelace :: Double -> Micro  -> Int64
usdToLovelace usd adaUsdPrice = round $ 1000000 * realToFrac usd / adaUsdPrice

-- | Cancel all pending subscriptions for a profile.
-- returns the number of deleted subscriptions
cancelPendingSubscription :: MonadSelda m => ID Profile -> m Int
cancelPendingSubscription pid = do
  deleteFrom subscriptions
    (\s -> s ! #subscriptionProfileId .== literal pid .&& s ! #subscriptionStatus .== literal PendingSubscription)

-- | Get all profile subscriptions
-- If justEnabled is True, only active subscriptions are returned
-- NOTE: expiration is not taken into account
getProfileSubscriptions :: MonadSelda m => ID Profile -> Bool -> m [SubscriptionDTO]
getProfileSubscriptions pid justEnabled = do
  subscription <- query $ do
    subscription <- select subscriptions
    restrict (subscription ! #subscriptionProfileId .== literal pid
             .&& ( if justEnabled then subscription ! #subscriptionStatus .== literal ActiveSubscription else true))
    pure subscription

  mapM toSubscriptionDTO subscription

getCurrentFeatures :: MonadSelda m => ID Profile -> UTCTime -> m [FeatureType]
getCurrentFeatures pid now = do
  -- get all the featureTypes of the selected tiers
  featureTypes <- query $ do
    tierFeature <- select tierFeatures
    -- join with the tiers table
    tier <- innerJoin
      (\t -> t ! #tierId .== tierFeature ! #tierFeatureTierId)
      (select tiers)
    -- join with subscription table
    subscription <- innerJoin
      (\s -> s ! #subscriptionTierId .== tier ! #tierId)
      (select subscriptions)
    restrict (subscription ! #subscriptionProfileId .== literal pid
             .&& subscription ! #subscriptionStatus .== literal ActiveSubscription
             .&& subscription ! #subscriptionEndDate .>= literal now)
    -- return just the featureType
    pure (tierFeature ! #tierFeatureFeatureId)
  -- unique the featureTypes
  pure $ nub featureTypes

toSubscriptionDTO :: MonadSelda m => Subscription -> m SubscriptionDTO
toSubscriptionDTO (Subscription{..}) = do
  -- get all the features of the selected tier
  features' <- query $ do
    tierFeature <- select tierFeatures
    restrict (tierFeature ! #tierFeatureTierId .== literal subscriptionTierId)
    -- join with the features table
    innerJoin
      (\feature -> feature ! #featureId .== tierFeature ! #tierFeatureFeatureId)
      (select features)
  pure SubscriptionDTO
    { subscriptionDtoId = subscriptionId
    , subscriptionDtoProfileId = subscriptionProfileId
    , subscriptionDtoTierId = subscriptionTierId
    , subscriptionDtoPrice = subscriptionPrice
    , subscriptionDtoStartDate = subscriptionStartDate
    , subscriptionDtoEndDate = subscriptionEndDate
    , subscriptionDtoStatus = subscriptionStatus
    , subscriptionDtoName = subscriptionName
    , subscriptionDtoFeatures = features'
    , subscriptionDtoType = subscriptionType
    }

getAllTiers :: MonadSelda m => m [TierDTO]
getAllTiers = do
  tiers' <- query $ select tiers
  mapM toTierDTO tiers'
  where
  toTierDTO :: MonadSelda m => Tier -> m TierDTO
  toTierDTO Tier{..} = do
    -- get all the features of the selected tier
    features' <- query $ do
      tierFeature <- select tierFeatures
      restrict (tierFeature ! #tierFeatureTierId .== literal tierId)
      -- join with the features table
      innerJoin
        (\feature -> feature ! #featureId .== tierFeature ! #tierFeatureFeatureId)
        (select features)
    pure TierDTO
      { tierDtoFeatures = features'
      , tierDtoTier = Tier{..}
      }

--TODO: replace this with a proper configuration
withDb :: (MonadIO m, MonadMask m) => SeldaT SQLite m a -> m a
withDb = withSQLite "certification.sqlite"
