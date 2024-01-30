{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module IOHK.Certification.Persistence.API where

import Control.Monad
import Data.Maybe
import Data.List (nub)
import Database.Selda hiding (Set)
import Database.Selda.Backend hiding (withConnection)
import IOHK.Certification.Persistence.Structure.Profile
import IOHK.Certification.Persistence.Structure.Subscription as Subscription
import IOHK.Certification.Persistence.Structure.Run
import IOHK.Certification.Persistence.Structure.Internal
import IOHK.Certification.Persistence.Structure
import IOHK.Certification.Persistence.Pattern
import Data.Time.Clock
import Data.Fixed
import Data.Int
import Data.Bifunctor
import Data.Functor

import qualified Data.Map as Map
import IOHK.Certification.Persistence.API.Run
import IOHK.Certification.Persistence.API.Profile
import IOHK.Certification.Persistence.API.Invoicing


getTransactionIdQ:: Text -> Query t (Col t (ID Transaction))
getTransactionIdQ  externalAddress = do
  p <- select transactions
  restrict (p ! #wtxExternalId .== text externalAddress)
  pure (p ! #wtxId)

getTransactionId :: MonadSelda m => Text -> m (Maybe (ID Transaction))
getTransactionId = fmap listToMaybe . query . getTransactionIdQ

getAllTransactionStatuses :: MonadSelda m => m [(ID Transaction, Text, TxStatus)]
getAllTransactionStatuses = do
  ret <- query $ do
        tx <- select transactions
        pure ( tx ! #wtxId :*: (tx ! #wtxExternalId) :*: tx ! #wtxStatus)
  pure $ map (\(a :*: b :*: c) -> (a,b,c)) ret

deleteTransaction :: MonadSelda m => ID Transaction -> m Int
deleteTransaction txId = do
  deleteFrom transactions (\tx -> tx ! #wtxId .== literal txId)

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

jwtSecretPropName :: Text
jwtSecretPropName = "jwt-secret"

getJWTSecret :: MonadSelda m => m (Maybe Text)
getJWTSecret = getLookupValue jwtSecretPropName

insertJWTSecret :: (MonadSelda m,MonadMask m) => Text -> m ()
insertJWTSecret = upsertLookupValue jwtSecretPropName

getLookupValueQ :: Text -> Query t (Col t Text)
getLookupValueQ property = do
  row' <- select lookupValues
  restrict (row' ! #lookupProp .== literal property)
  pure (row' ! #lookupValue)

getLookupValue :: MonadSelda m => Text -> m (Maybe Text)
getLookupValue property = fmap listToMaybe $
  query $ getLookupValueQ property

upsertLookupValue :: (MonadSelda m,MonadMask m) => Text -> Text -> m ()
upsertLookupValue property value = do
  void $ upsert lookupValues
     (\p -> p ! #lookupProp .== literal property)
     (`with` [ #lookupValue := literal value ])
     [Lookup property value]

getPendingSubscriptions :: MonadSelda m => m [Subscription]
getPendingSubscriptions = query $ do
  sub <- select subscriptions
  restrict (sub ! #subscriptionStatus .== literal PendingSubscription)
  pure sub

activateAllPendingSubscriptions :: MonadSelda m => VatPercentage -> m [ID Subscription]
activateAllPendingSubscriptions vat = do
  updates <- mapM (activateSubscription vat) =<< getPendingSubscriptions
  pure $ catMaybes updates

activateSubscription :: MonadSelda m
                     => Int64
                     -> Subscription
                     -> m (Maybe (ID Subscription))
activateSubscription vat sub = do
  profile <- getProfile (sub.subscriptionProfileId)
  case profile of
    Just p -> do
      balance <- fromMaybe 0 <$> getProfileBalance (p.profile.ownerAddress)
      if balance >= sub.subscriptionPrice
      then do
        update_ subscriptions
          (\s -> s ! #subscriptionId .== literal (sub.subscriptionId))
          (`with` [ #subscriptionStatus := literal ActiveSubscription])
        -- try to invoice the subscription
        _ <- createSubscriptionInvoice (sub.subscriptionId) vat
        pure $ Just (sub.subscriptionId)
      else
        pure Nothing
    Nothing -> pure Nothing

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
-- | NOTE: the actual sum is not computed from the DB-stored transactions anymore
-- | but synchronized from the synchronizer thread. see: ProfileWallet outside
-- | of the persistence layer. TODO: this might be refactored in the future
getProfileBalance :: MonadSelda m => ProfileWalletAddress -> m (Maybe Int64)
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
      walletIncomingCredits <- getProfileWallet pid <&> getWalletCredits
      -- sum all the costs of the certified runs
      let certifiedCosts = sum $ map certificationPrice certifiedRuns
          -- calculate the amount of credits available for subscriptions
          subscriptionCredits = sum $ map subscriptionPrice paidSubscriptions
          -- calculate the amount of credits available
          creditsAvailable = walletIncomingCredits - fromIntegral certifiedCosts - fromIntegral subscriptionCredits
      pure $ Just creditsAvailable
  where
  getWalletCredits :: Maybe (Profile, Maybe ProfileWallet) -> Int64
  getWalletCredits Nothing = 0
  getWalletCredits (Just (_, Nothing)) = 0
  getWalletCredits (Just (_, Just ProfileWallet{..})) = profileWalletCredits

addInitialData :: MonadSelda m => m ()
addInitialData = void $ do
  let devFeatures = [ Feature L1Run "Testing of a DApp with Property Based Testing" ]
      auditFeatures = [ Feature L2UploadReport "CIP-0096 metadata generation for audit reports" ]
  insert_ features (devFeatures <> auditFeatures)
  -- TODO: price should be configurable
  let usdPrice = 2
  -- TODO: subscription duration should be configurable
  let developer =
        [ Tier def "Standard" "All Testing Features"
          "This tier is suited for developers that want to test one DApp.\
          \ With this tier, you will have access to our testing tool and all its features as described in the documentation."
          Developer usdPrice 365 True ]
      auditor  =
        [Tier def "Auditor" "All Testing Features \
        \and CIP-96 formatting of Reports "
         "This tier is suited for auditors who want to test multiple DApps and format reports to be CIP-0096 compliant.\
         \ With this tier, you will have access to our testing tool and all its features as described in the documentation.\
         \ You will also be able to format your reports to be CIP-0096 compliant."
        Auditor usdPrice 365 True]
  developerId <- insertWithPK tiers developer
  auditorId <- insertWithPK tiers auditor
  insert_ tierFeatures  [TierFeature def developerId (featureId f) | f <- devFeatures]
  insert_ tierFeatures  [TierFeature def auditorId (featureId f) | f <- devFeatures <> auditFeatures]

-- | Create a subscription for a profile based on a tier.
-- creates a pending subscription and disables all other subscriptions for this profile
createSubscription :: MonadSelda m
                   => UTCTime
                   -> ID Profile
                   -> ID Tier
                   -> Micro
                   -> m (Maybe SubscriptionDTO)
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
          subscriptionPrice (realToFrac adaUsdPrice) startDate endDate PendingSubscription

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
    , subscriptionDtoAdaUsdPrice = subscriptionAdaUsdPrice
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

type JustOutput = Bool


data MinimalTransaction = MinimalTransaction
                        { mtxTxId :: !(ID Transaction)
                        , mtxAmount :: !Int64
                        , mtxMetadata :: !Text
                        }
data MinimalTransactionEntry = MinimalTransactionEntry
                        { mteId :: !(ID TransactionEntry)
                        , mteAddress :: !Text
                        , mteInput :: !Bool
                        }

getAllTransactions :: MonadSelda m => JustOutput -> m [(MinimalTransaction,[MinimalTransactionEntry])]
getAllTransactions justOutput = do
  tx <- query $ do
    t <- select transactions
    order (t ! #wtxTime) ascending
    pure t
  entries <- query $ do
    e <- select transactionEntries
    when justOutput $
      restrict (e ! #txEntryInput .== literal justOutput)
    pure e
  -- make a map of entries by txId
  let entriesMap = Map.fromListWith (<>) $ map (\e -> (txEntryTxId e, [e])) entries
  -- link the entries to the transactions
  let txs = map (\t -> (t, Map.findWithDefault [] (wtxId t) entriesMap)) tx
  -- convert to the minimal representation
  pure $ map (bimap toMinimalTransaction (map toMinimalTransactionEntry)) txs
    where
    toMinimalTransactionEntry :: TransactionEntry -> MinimalTransactionEntry
    toMinimalTransactionEntry TransactionEntry{..} = MinimalTransactionEntry
      { mteId = txEntryId
      , mteAddress = txEntryAddress
      , mteInput = txEntryInput
      }
    toMinimalTransaction :: Transaction -> MinimalTransaction
    toMinimalTransaction Transaction{..} = MinimalTransaction
      { mtxTxId = wtxId
      , mtxAmount = wtxAmount
      , mtxMetadata = wtxMetadata
      }

getSubscriptionsStartingInInterval :: MonadSelda m => UTCTime -> UTCTime -> m [SubscriptionDTO]
getSubscriptionsStartingInInterval start end = do
  subscription <- query $ do
    subscription <- select subscriptions
    restrict ( subscription ! #subscriptionStartDate .>= literal start
             .&& subscription ! #subscriptionStartDate .< literal end)
    pure subscription

  mapM toSubscriptionDTO subscription

getSubscriptionsEndingInInterval :: MonadSelda m => UTCTime -> UTCTime -> m [SubscriptionDTO]
getSubscriptionsEndingInInterval start end = do
  subscription <- query $ do
    subscription <- select subscriptions
    restrict ( subscription ! #subscriptionEndDate .>= literal start
             .&& subscription ! #subscriptionEndDate .< literal end
             .&& subscription ! #subscriptionStatus .== literal ActiveSubscription
             )
    pure subscription

  mapM toSubscriptionDTO subscription

-- | Polimorphic function to run a Selda computation with a connection
withConnection :: (MonadIO m, MonadMask m)
               => SeldaConnection b
               -> (forall n. (MonadSelda n,MonadMask n) => n a)
               -> m a
withConnection = flip runSeldaT

