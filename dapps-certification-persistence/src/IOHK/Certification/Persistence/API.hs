{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
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
import IOHK.Certification.Persistence.Structure.Certification
import IOHK.Certification.Persistence.Structure
import IOHK.Certification.Persistence.Pattern
import Data.Time.Clock
import Data.Fixed
import Data.Int
import Data.Bifunctor
import Data.Functor
import Data.Set (Set,toList)

import qualified Data.Map as Map

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
deleteTransaction txIds = do
  deleteFrom transactions (\tx -> tx ! #wtxId .== literal txIds)

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

getAllCertifiedRunsForAddress :: MonadSelda m => ProfileWalletAddress -> m [Run]
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

upsertProfile :: (MonadSelda m, MonadMask m) => Profile -> Maybe DApp -> m (Maybe (ID Profile))
upsertProfile profile@Profile{..} dappM = do
  void $ upsert profiles
     (\p -> p ! #ownerAddress .== literal ownerAddress)
     (`with`
      [ #website      := fromMaybe' website
      , #twitter      := fromMaybe' twitter
      , #linkedin     := fromMaybe' linkedin
      , #email        := fromMaybe' email
      , #contactEmail := fromMaybe' contactEmail
      , #companyName  := fromMaybe' companyName
      , #fullName     := fromMaybe' fullName
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
           , #dappVersion := literal dappVersion
           , #dappId := literal profileId
           , #dappGitHubToken := literal dappGitHubToken
           , #dappSubject := literal dappSubject
           ]
          )
          [dapp { dappId = pid }]
  pure profileIdM
  where
  fromMaybe' :: SqlType a => Maybe a -> Col s (Maybe a)
  fromMaybe' = maybe null_ (just . literal)

getProfileByAddress :: MonadSelda m => ProfileWalletAddress -> m (Maybe Profile)
getProfileByAddress address = listToMaybe <$> query (do
    p <- select profiles
    restrict (p ! #ownerAddress .== literal address)
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

getProfileWalletQ :: ProfileId -> Query t (Row t Profile :*: Row t (Maybe ProfileWallet))
getProfileWalletQ profileId = do
  p <- select profiles
  restrict (p ! #profileId .== literal profileId)
  profileWallet <- leftJoin  (\pw -> pw ! #profileWalletId .== p ! #profileId) (select profileWallets)
  pure (p :*: profileWallet)

getProfileWallet :: MonadSelda f => ProfileId -> f (Maybe (Profile , Maybe ProfileWallet))
getProfileWallet profileId = fmap toTuple . listToMaybe <$> query (getProfileWalletQ profileId)

toTuple ::  a :*: b -> (a, b)
toTuple (p :*: pw) = (p,pw)

getProfileWalletsQ :: Query t (Row t Profile :*: Row t (Maybe ProfileWallet))
getProfileWalletsQ = do
  p <- select profiles
  profileWallet <- leftJoin  (\pw -> pw ! #profileWalletId .== p ! #profileId) (select profileWallets)
  pure (p :*: profileWallet)

getProfileWallets :: MonadSelda m => m [(Profile, Maybe ProfileWallet)]
getProfileWallets = map toTuple <$> query getProfileWalletsQ

upsertProfileWallet :: (MonadSelda m,MonadMask m) => ProfileWallet -> m ()
upsertProfileWallet ProfileWallet{..} = do
  void $ upsert profileWallets
    (\pw -> pw ! #profileWalletId .==  literal profileWalletId)
    (`with`
     [ #profileWalletAddress := text profileWalletAddress
     , #profileWalletStatus := literal profileWalletStatus
     , #profileWalletCredits := literal profileWalletCredits
     ])
    [ProfileWallet{..}]

getProfile :: MonadSelda m => ID Profile -> m (Maybe ProfileDTO)
getProfile pid = do
   ret <- listToMaybe <$> query (getProfileQ pid)
   userRoles <- getUserRoles pid
   maxUserRole <- case userRoles of
      _ | null userRoles || userRoles == [NoRole] -> pure Nothing
      _ | userRole <- maximum userRoles -> pure $ Just userRole
   pure $ ret <&> \(r :*: dapp) ->
    ProfileDTO r (DAppDTO <$> dapp) maxUserRole

getProfileDApp :: MonadSelda m => ID Profile -> m (Maybe DApp)
getProfileDApp pid = fmap listToMaybe $ query $ getProfileDAppQ pid

getProfileIdQ:: ProfileWalletAddress -> Query t (Col t (ID Profile))
getProfileIdQ  address = do
  p <- select profiles
  restrict (p ! #ownerAddress .== literal address)
  pure (p ! #profileId)

getProfileId :: MonadSelda m => ProfileWalletAddress -> m (Maybe ProfileId)
getProfileId = fmap listToMaybe . query . getProfileIdQ

getProfileAddressQ :: ID Profile -> Query t (Col t ProfileWalletAddress)
getProfileAddressQ  pid = do
  p <- select profiles
  restrict (p ! #profileId .== literal pid)
  pure (p ! #ownerAddress)

getProfileAddress :: MonadSelda m => ID Profile -> m (Maybe ProfileWalletAddress)
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

--------------------------------------------------------------------------------
-- | USER ROLES

-- | Add a role to a profile
addUserRole :: MonadSelda m => ID Profile -> UserRole -> m Int
addUserRole pid role = insert profileRoles [ProfileRole pid role]

updateUserRoles :: MonadSelda m => ID Profile -> Set UserRole -> m Int
updateUserRoles pid roles = do
  -- remove all the roles
  _ <- removeAllUserRoles pid
  -- add the new roles
  insert profileRoles (map (ProfileRole pid) (toList  roles))

-- | Get all the roles for a profile
getUserRoles :: MonadSelda m => ID Profile -> m [UserRole]
getUserRoles pid = query $ do
  role <- select profileRoles
  restrict (role ! #profileId .== literal pid)
  pure (role ! #role)

-- | Check if a profile has at least one of the given roles
hasSomeUserRoles :: MonadSelda m => ID Profile -> [UserRole] -> m Bool
hasSomeUserRoles pid roles = do
  userRoles <- getUserRoles pid
  pure $ any (`elem` roles) userRoles

-- | Check if a profile has at least a given role level
-- e.g.
--
-- 1. Profile (Support)
-- hasAtLeastUserRole pid Support == True
-- hasAtLeastUserRole pid Admin == False
--
-- 2. Profile (Admin)
-- hasAtLeastUserRole pid Support == True
-- hasAtLeastUserRole pid Admin == True
--
hasAtLeastUserRole :: MonadSelda m => ID Profile -> UserRole -> m Bool
hasAtLeastUserRole pid role = do
  roles <- query $ hasAtLeastUserRole' pid role
  pure $ not $ null roles

hasAtLeastUserRole' :: ID Profile -> UserRole -> Query t (Col t UserRole)
hasAtLeastUserRole' pid role = do
  role' <- select profileRoles
  restrict (role' ! #profileId .== literal pid
       .&& role' ! #role .>= literal role)
  pure (role' ! #role)

-- | Remove all the roles for a profile
-- returns the number of deleted roles
removeAllUserRoles :: MonadSelda m => ID Profile -> m Int
removeAllUserRoles pid =
  deleteFrom profileRoles (\role -> role ! #profileId .== literal pid)

-- | Remove a role for a profile
-- returns True if the role was removed
-- returns False if the role was not found
removeUserRole :: MonadSelda m => ID Profile -> UserRole -> m Bool
removeUserRole pid role = do
  deleted <- deleteFrom profileRoles (\role' -> role' ! #profileId .== literal pid
         .&& role' ! #role .== literal role)
  pure $ deleted > 0

ensureAdminExists :: (MonadSelda m) => Bool -> ProfileWalletAddress -> m Int
ensureAdminExists forceAdminAlways walletAddress = do
  -- first ensure there is at least one admin
  admins <- query $ do
    role <- select profileRoles
    restrict (role ! #role .== literal Admin)
    pure (role ! #profileId)
  -- get the profile
  profileM <- getProfileByAddress walletAddress
  case profileM of
    Just profile
      -- if there are no admins
      | null admins
      -- or when forceAdminAlways is True and the profile is not an admin
      || (forceAdminAlways && (profile.profileId `notElem` admins)) ->
         -- add the admin role to the profile and return the number of admins
         -- (including the new one)
         (+ length admins) <$> addUserRole (profile.profileId) Admin
    _ -> pure (length admins)

getAllProfilesByRole :: MonadSelda m => UserRole -> m [ID Profile]
getAllProfilesByRole userRole = query $ do
  role <- select profileRoles
  restrict (role ! #role .== literal userRole)
  pure (role ! #profileId)

data Impersonation
  = ImpersonationNotAllowed
  | ImpersonationProfile (ID Profile, ProfileWalletAddress)
  | ImpersonationNotFound

-- verify minimum role and return the profileId
-- and the address of the impersonated user
verifyImpersonation :: MonadSelda m
                    => ID Profile
                    -> UserRole
                    -> ID Profile
                    -> m Impersonation
verifyImpersonation ownerPid role pid = do
  -- check if the impersonated user has at least the given role
  hasRole <- hasAtLeastUserRole ownerPid role
  if hasRole
  then do
    -- get the address of the impersonated user
    addressM <- getProfileAddress pid
    case addressM of
      Nothing -> pure ImpersonationNotFound
      Just address -> pure $ ImpersonationProfile (pid, address)
  else pure ImpersonationNotAllowed

--------------------------------------------------------------------------------
-- | Polimorphic function to run a Selda computation with a connection
withConnection :: (MonadIO m, MonadMask m)
               => SeldaConnection b
               -> (forall n. (MonadSelda n,MonadMask n) => n a)
               -> m a
withConnection = flip runSeldaT

