{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE QuasiQuotes          #-}

module Plutus.Certification.Synchronizer
  ( startTransactionsMonitor
  , SynchronizerSelector(..)
  , renderSynchronizerSelector
  -- TODO: remove this export, we are using this just to suppress warnings
  , certifyRuns
  ) where

import Plutus.Certification.WalletClient.Transaction
import Plutus.Certification.WalletClient
import Plutus.Certification.Internal
import Control.Concurrent (threadDelay)
import Data.Time (UTCTime)
import Data.ByteString (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad.Catch (MonadMask, catchAll)
import Data.List (groupBy)
import Plutus.Certification.API.Routes (RunIDV1(..))
import Plutus.Certification.CoinGeckoClient
import Data.Aeson
import Data.Aeson.QQ
import Plutus.Certification.CertificationBroadcaster
import Observe.Event.Render.JSON
import Control.Exception
import Observe.Event.Crash
import Data.Function (on)
import Data.UUID (UUID)
import Control.Monad.Except
import Data.Maybe
import Observe.Event.Backend
import Observe.Event
import Data.Void
import Control.Monad.RWS
import Control.Monad.Reader (ReaderT(..))
import Observe.Event.BackendModification (setAncestor)
import Plutus.Certification.ProfileWallet
import Data.IORef
import Data.Word (Word64)
import qualified Data.HashSet as HashSet

import qualified Plutus.Certification.WalletClient as Wallet
import qualified IOHK.Certification.Persistence as DB
import qualified Data.Set as Set

data InitializingField
  = WalletArgsField WalletArgs
  | DelayField Int
  | ErrorField SomeException

data SynchronizeTransactionsField
  = DeletedTransactionsField [Text]
  | StillPendingTransactionsField [Text]
  | UpdatedTransactionsField [Text]
  | AllTransactionsField Int
  | FirstSyncField Bool

data SynchronizerSelector f where
  InitializingSynchronizer :: SynchronizerSelector InitializingField
  InjectTxBroadcaster :: forall f . !(TxBroadcasterSelector f) -> SynchronizerSelector f
  InjectCoinGeckoClient :: forall f . !(CoinGeckoClientSelector f) -> SynchronizerSelector f
  InjectProfileWalletSync :: forall f . !(ProfileWalletSyncSelector f) -> SynchronizerSelector f
  MonitorTransactions :: SynchronizerSelector TransactionsCount
  ActivateSubscriptions :: SynchronizerSelector [DB.SubscriptionId]
  UpdateAdaPrice :: SynchronizerSelector Void
  SynchronizeTransactions :: SynchronizerSelector SynchronizeTransactionsField

newtype TransactionsCount = TransactionsCount Int

renderSynchronizerSelector :: RenderSelectorJSON SynchronizerSelector
renderSynchronizerSelector InitializingSynchronizer =
  ( "synchronizer-initializing"
  , \case
      WalletArgsField WalletArgs{..} -> ("wallet-args", object
            [ "id" .= walletId
            , "address" .=  walletAddress
            , "apiAddress" .=  walletAPIAddress
            , "certificationPrice" .= walletCertificationPrice
            ])
      DelayField delay -> ("delay", toJSON delay)
      ErrorField err -> ("error", toJSON $ show err)
  )
renderSynchronizerSelector (InjectTxBroadcaster selector) = renderTxBroadcasterSelector selector
renderSynchronizerSelector (InjectCoinGeckoClient selector) = renderCoinGeckoClientSelector selector
renderSynchronizerSelector MonitorTransactions = ("monitor-transactions", renderTransactionsCount)
renderSynchronizerSelector ActivateSubscriptions = ("activate-subscriptions", renderSubscriptions)
renderSynchronizerSelector UpdateAdaPrice = ("refresh-ada-price", absurd)
renderSynchronizerSelector (InjectProfileWalletSync selector) = renderProfileWalletSyncSelector selector
renderSynchronizerSelector SynchronizeTransactions =
  ( "synchronize-transactions"
  , \case
      DeletedTransactionsField deleted -> ("deleted-transactions", toJSON deleted)
      StillPendingTransactionsField stillPending -> ("still-pending-transactions", toJSON stillPending)
      AllTransactionsField allTx -> ("all-transactions", toJSON allTx)
      FirstSyncField firstSync -> ("first-sync", toJSON firstSync)
      UpdatedTransactionsField updated ->
        let len = length updated
            truncated = take maxLen updated
            maxLen = 20
        in ("updated-transactions", [aesonQQ|{
              txIds: #{truncated },
              length: #{len},
              truncated: #{len > maxLen}
            }|])
  )
renderTransactionsCount :: RenderFieldJSON TransactionsCount
renderTransactionsCount (TransactionsCount count) = ("transactions-count",toJSON count)

renderSubscriptions :: RenderFieldJSON [DB.SubscriptionId]
renderSubscriptions subscriptions = ("subscriptions", toJSON subscriptions)

getTimeFromTx :: WalletTransaction -> Maybe UTCTime
getTimeFromTx (WalletTransaction _ status)=
  -- try to extract timestamp from transaction
  -- based on status and inserted_at , pending_since or expired_at
  case status of
    Pending pending_since _ -> Just $ pending_since.apiSlotReference.time
    Expired expired_at -> Just $ expired_at.apiSlotReference.time
    InLedger inserted_at -> Just $ inserted_at.apiSlotReference.time
    Submitted -> Nothing

walletTxStatusToDbStatus :: StatusWithData a -> DB.TxStatus
walletTxStatusToDbStatus (Pending _ _) = DB.Pending
walletTxStatusToDbStatus (Expired _) = DB.Expired
walletTxStatusToDbStatus (InLedger _) = DB.InLedger
walletTxStatusToDbStatus Submitted = DB.Submitted

synchronizeDbTransactions :: (MonadIO m, MonadMask m,MonadReader env m,HasDb env)
                          => EventBackend m r SynchronizerSelector
                          -> [WalletTransaction]
                          -> IORef Bool
                          -> m ()
synchronizeDbTransactions eb transactions firstSyncRef =
  withEvent eb SynchronizeTransactions $ \ev -> do
  -- See if it's the first time we synchronize the transactions.
  -- In that case we will update all transactions no matter what.
  -- In this way if we have any future bugs, by restarting the service
  -- we won't miss any transactions
  firstSync <- liftIO $ readIORef firstSyncRef
  addField ev $ FirstSyncField firstSync
  -- set the flag to false if it's the first time we synchronize
  when firstSync $ liftIO $ writeIORef firstSyncRef False

  (allDbTx,deleted,stillPending,updated) <- processTransactions firstSync

  --TODO: instead of fetching all the metrics from the processTransactions
  -- learn how to hoist the event backend and use the event there
  addField ev $ AllTransactionsField allDbTx
  addField ev $ DeletedTransactionsField deleted
  addField ev $ StillPendingTransactionsField $ Set.toList stillPending
  addField ev $ UpdatedTransactionsField updated

  where
  processTransactions firstSync = withDb $ do
    -- first let's get all transaction statuses
    dbTxStatuses <- DB.getAllTransactionStatuses
    let (present,gone) = partitionTransactions dbTxStatuses
    -- delete all gone transactions
    let (goneIds,goneExternalIds) =
         unzip $ map (\(txId,txExtId,_) -> (txId,txExtId)) gone
    forM_ goneIds DB.deleteTransaction

    let stillPendingSet = Set.fromList
          $ map (\(_,ix,_) -> ix)
          $ filter (\(_,_,status) -> status /= DB.InLedger) present
        inLedger = Set.fromList (map (\(_,txId,_) -> txId) present)
          `Set.difference` stillPendingSet

    -- update only the present ones with status not InLedger
    -- and insert the new ones
    -- NOTE: firstSync is used to update all transactions so we pass an empty set
    -- for the inLedger transactions. This will result in updating all transactions
    updates <- forM transactions (processTransaction (if firstSync then Set.empty else inLedger))
    -- fold updates from [Maybe x] to [x]
    pure ( length dbTxStatuses
         , goneExternalIds
         , stillPendingSet
         , catMaybes updates
         )

  walletTxIds = Set.fromList (map (txId . walletTxId . walletTxData) transactions)
  partitionTransactions = Prelude.foldl f ([],[])
    where
    f (stillPresent,notPresent) tx@(_,txExtId,_)= if Set.member txExtId walletTxIds
      then (tx:stillPresent,notPresent)
      else (stillPresent,tx:notPresent)

  processTransaction inLedger tx@WalletTransaction{..} = do
    let txId' = walletTxData.walletTxId.txId

    case (getTimeFromTx tx,Set.member txId' inLedger) of
      -- if the transaction does not have a time, is submitted
      (Nothing,_) -> return Nothing
      -- if the transaction has a time and it's already marked as InLedger
      -- in the database, we don't need to update it
      (Just _,True) -> return Nothing
      -- if the transaction has a time and it's still pending or it is new
      -- we need to upsert it
      (Just time,_) -> do
        let dbTx  = DB.Transaction
                  { DB.wtxId         = undefined
                  , DB.wtxExternalId = txId'
                  -- IMPORTANT: based on the direction of the transaction
                  -- we set the amount to be positive or negative
                  , DB.wtxAmount = amountDirection * fromIntegral ( walletTxData.walletTxAmount.quantity )
                  , DB.wtxTime = time
                  , DB.wtxDepth = maybe (-1) quantity (walletTxData.walletTxDepth)
                  , DB.wtxStatus = walletTxStatusToDbStatus walletTxStatus
                  , DB.wtxMetadata   = case walletTxData.walletTxMetadata of
                      Nothing -> ""
                      Just val -> decodeUtf8 . toStrict . encode $ val
                  }
            amountDirection = if walletTxData.walletTxDirection == Incoming then 1 else (-1)
            inputEntries = fromInputsToDbInputs walletTxData.walletTxInputs
            outputEntries = fromOutputsToDbOutputs walletTxData.walletTxOutputs
        -- store the transaction in the database
        _ <- DB.upsertTransaction dbTx (inputEntries ++ outputEntries)
        return $ Just txId'

fromOutputsToDbOutputs :: [TxOutput] -> [DB.TransactionEntry]
fromOutputsToDbOutputs = foldl (\acc output -> case fromOutputToDbOutput output of
  Nothing -> acc
  Just dbOutput -> dbOutput:acc) []

fromOutputToDbOutput :: TxOutput -> Maybe DB.TransactionEntry
fromOutputToDbOutput TxOutput{..} = Just $ DB.TransactionEntry
  { DB.txEntryId = undefined
  , DB.txEntryTxId = undefined
  , DB.txEntryAddress = txOutputAddress.unPublicAddress
  --TODO: remove conversion after merging with feat/subscription
  , DB.txEntryAmount = fromIntegral txOutputAmount.quantity
  , DB.txEntryIndex = Nothing
  , DB.txEntryInput = False
  }

fromInputsToDbInputs :: [TxInput] -> [DB.TransactionEntry]
fromInputsToDbInputs = foldl (\acc input -> case fromInputToDbInput input of
  Nothing -> acc
  Just dbInput -> dbInput:acc) []

fromInputToDbInput :: TxInput -> Maybe DB.TransactionEntry
fromInputToDbInput (TxInput _ _ Nothing) = Nothing
fromInputToDbInput (TxInput index _ (Just TxOutput{..}))
  = Just $ DB.TransactionEntry
  { DB.txEntryId = undefined
  , DB.txEntryTxId = undefined
  , DB.txEntryAddress = txOutputAddress.unPublicAddress
  --TODO: remove conversion after merging with feat/subscription
  , DB.txEntryAmount = fromIntegral txOutputAmount.quantity
  , DB.txEntryIndex = Just index
  , DB.txEntryInput = True
  }

monitorWalletTransactions :: (MonadIO m, MonadMask m,MonadError IOException m,MonadReader env m,HasDb env)
                          => EventBackend m r SynchronizerSelector
                          -> WalletArgs
                          -> Word64
                          -> IORef PrevAssignments
                          -> IORef Bool
                          -> m ()
monitorWalletTransactions eb args minAssignmentAmount refAssignments firstSyncRef =
  withEvent eb MonitorTransactions $ \ev -> do

  -- fetch the list of transactions from the wallet
  -- TODO: fetch only the transactions that are not in the database
  -- or starting from the first pending transaction
  transactions <-  getTransactionList wc >>= handleResponse
  addField ev $ TransactionsCount $ length transactions
  synchronizeDbTransactions (subEventBackend ev) transactions firstSyncRef
  activateSubscriptions (subEventBackend ev)
  -- synchronize wallets
  isOurAddress <- getIsOurAddress
  liftIO (readIORef refAssignments)
    >>= resyncWallets (narrowEventBackend InjectProfileWalletSync eb) wc isOurAddress minAssignmentAmount
    >>= liftIO . writeIORef refAssignments

  where
    wc :: WalletClient
    wc = realClient args
    -- handle the response from the wallet
    handleResponse (Left err) = do
      throwError (userError $ "Error while fetching transactions: " ++ show err)
    handleResponse (Right transactions) = return transactions
    getIsOurAddress = do
      resp <- liftIO $ Wallet.getWalletAddresses wc Nothing
      case resp of
        Left err -> throwError (userError $ "Error while fetching addresses: " ++ show err)
        Right addresses -> do
          let set = HashSet.fromList $ map addressId addresses
          return $ \addr -> HashSet.member addr set

type CertificationProcess m = DB.ProfileId -> UUID -> m DB.L1CertificationDTO

-- certify all runs who have enough credit to be certified
-- and have not been certified yet
-- NOTE: this is momentarily disabled
certifyRuns :: (MonadIO m, MonadMask m,MonadError IOException m,MonadReader env m,HasDb env)
            => EventBackend m r SynchronizerSelector
            -> WalletClient
            -> m ()
certifyRuns eb wc = do
  -- fetch the list of runs from the database
  runs <- withDb DB.getRunsToCertify

  -- group runs by profileId
  let runsByProfile = groupBy ((==) `on` (.profileId)) runs

  -- for each profile runs, certify them
  -- TODO: parallelize this
  forM_ runsByProfile $ certifyProfileRuns certificationProcess
  where
  certificationProcess a b = createL1Certification
    ( narrowEventBackend InjectTxBroadcaster eb ) wc a (RunID b)

activateSubscriptions :: (MonadIO m, MonadMask m,MonadError IOException m,MonadReader env m,HasDb env)
                      => EventBackend m r SynchronizerSelector
                      -> m ()
activateSubscriptions eb = withEvent eb ActivateSubscriptions $ \ev -> do
  -- activate all pending subscriptions with enough credits
  withDb DB.activateAllPendingSubscriptions >>= addField ev

-- certify all runs of a profile
certifyProfileRuns :: (MonadIO m, MonadMask m,MonadReader env m,HasDb env)
                   => CertificationProcess m
                   -> [DB.Run]
                   -> m ()
certifyProfileRuns certificationProcess runs =
  -- get the profile
  withDb (DB.getProfileAddress pid)
    -- and certify the runs
    >>= mapM_ certifyProfileRuns'
  where
  pid = (head runs).profileId
  certifyProfileRuns' address = do
    -- calculate the amount of credits available
    creditsAvailable <- fromMaybe 0 <$> withDb (DB.getProfileBalance address)
    -- recursively certify the runs until we run out of credits
    certifyRuns' runs creditsAvailable

  certifyRuns' [] _ = return ()
  certifyRuns' (run:rs) creditsAvailable = do
    -- calculate the cost of the run
    let cost = fromIntegral run.certificationPrice
    -- if we have enough credits, certify the run
    when (creditsAvailable >= cost) $
      void (certificationProcess pid (run.runId))
    -- recursively certify the next runs
    certifyRuns' rs (creditsAvailable - cost)

-- | Start a thread that monitors the wallet transactions
-- and updates the database accordingly.
--
-- The thread will run forever, and will be restarted if it crashes.
-- The delay between each check is specified in seconds.
startTransactionsMonitor :: (MonadIO m, MonadMask m, MonadError IOException m,HasDb env)
                         => EventBackend m r SynchronizerSelector
                         -> ScheduleCrash m r
                         -> WalletArgs
                         -> IORef (Maybe DB.AdaUsdPrice)
                         -> Int
                         -> Word64
                         -> env
                         -> m ()
startTransactionsMonitor eb scheduleCrash args adaPriceRef delayInSeconds minAssignmentAmount = runReaderT reader'
  where
  reader' = startTransactionsMonitor' eb' scheduleCrash' args adaPriceRef
              delayInSeconds minAssignmentAmount
  eb' = hoistEventBackend (ReaderT . const) eb
  scheduleCrash' = hoistScheduleCrash (ReaderT . const) scheduleCrash

startTransactionsMonitor' :: (MonadIO m, MonadMask m, MonadError IOException m,MonadReader env m,HasDb env)
                         => EventBackend m r SynchronizerSelector
                         -> ScheduleCrash m r
                         -> WalletArgs
                         -> IORef (Maybe DB.AdaUsdPrice)
                         -> Int
                         -> Word64
                         -> m ()
startTransactionsMonitor' eb scheduleCrash args adaPriceRef delayInSeconds minAssignmentAmount =
  withEvent eb InitializingSynchronizer $ \ev -> do
    addField ev $ WalletArgsField args
    addField ev $ DelayField delayInSeconds
    -- TODO maybe a forkIO here will be better than into the calling function
    -- hence, now, the parent instrumentation event will never terminate
    catchAll
      (doWork ev)
      (catchAndCrash ev)
  where
    catchAndCrash ev (e :: SomeException) = do
        addField ev (ErrorField e)
        let mods = setAncestor $ reference ev
        schedule scheduleCrash mods
    doWork ev = do
      ref <- liftIO $ newIORef []
      firstSyncRef <- liftIO $ newIORef True
      void $ forever $ do
        updateAdaPrice (subEventBackend ev) adaPriceRef
        monitorWalletTransactions (subEventBackend ev) args minAssignmentAmount ref firstSyncRef
        liftIO $ threadDelay delayInMicroseconds
    delayInMicroseconds = delayInSeconds * 1000000

updateAdaPrice :: (MonadIO m,MonadMask m)
               => EventBackend m r SynchronizerSelector
               -> IORef (Maybe DB.AdaUsdPrice)
               -> m ()
updateAdaPrice eb ref = withEvent eb UpdateAdaPrice $ \_ -> do
  -- fetch the ada price from the wallet
  adaPrice <- getAdaPrice ( narrowEventBackend InjectCoinGeckoClient eb )
  prevValue <- liftIO $ readIORef ref
  liftIO $ writeIORef ref $
    case adaPrice of
     -- TODO: this is a temporary hack to avoid crashing any consumer of
     -- the ada price. 
     Left _ -> prevValue
     Right p -> Just p
