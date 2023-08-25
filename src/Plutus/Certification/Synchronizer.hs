{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Plutus.Certification.Synchronizer
  ( startTransactionsMonitor
  , SynchronizerSelector(..)
  , renderSynchronizerSelector
  ) where

import Plutus.Certification.WalletClient.Transaction
import Plutus.Certification.WalletClient
import Plutus.Certification.Internal
import Control.Concurrent (threadDelay)
import Data.Time (UTCTime)
import Data.ByteString (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad.Catch (MonadMask, catchAll)
import Data.List (groupBy)
import Plutus.Certification.API.Routes (RunIDV1(..))
import Plutus.Certification.CoinGeckoClient
import Data.Aeson
import Plutus.Certification.CertificationBroadcaster
import Observe.Event.Render.JSON
import Control.Exception
import Observe.Event.Crash
import Data.Function (on)
import Data.UUID (UUID)
import Control.Monad.Except
import Data.Maybe (fromMaybe)
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

data InitializingField
  = WalletArgsField WalletArgs
  | DelayField Int
  | ErrorField SomeException

data SynchronizerSelector f where
  InitializingSynchronizer :: SynchronizerSelector InitializingField
  InjectTxBroadcaster :: forall f . !(TxBroadcasterSelector f) -> SynchronizerSelector f
  InjectCoinGeckoClient :: forall f . !(CoinGeckoClientSelector f) -> SynchronizerSelector f
  InjectProfileWalletSync :: forall f . !(ProfileWalletSyncSelector f) -> SynchronizerSelector f
  MonitorTransactions :: SynchronizerSelector TransactionsCount
  ActivateSubscriptions :: SynchronizerSelector [DB.SubscriptionId]
  UpdateAdaPrice :: SynchronizerSelector Void

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

synchronizeDbTransactions :: (MonadIO m, MonadMask m,MonadReader env m,HasDb env) => [WalletTransaction] -> m ()
synchronizeDbTransactions transactions = do
  withDb $ forM_ transactions storeTransaction
  where
  storeTransaction tx@WalletTransaction{..} = void $
      case getTimeFromTx tx of
        -- if the transaction does not have a time, is submitted
        Nothing -> return Nothing
        -- if the transaction has a time, store it in the database
        Just time ->
          let dbTx  = DB.Transaction
                    { DB.wtxId         = undefined
                    , DB.wtxExternalId = walletTxData.walletTxId.txId
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
          in DB.upsertTransaction dbTx (inputEntries ++ outputEntries)

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
                          -> m ()
monitorWalletTransactions eb args minAssignmentAmount refAssignments = withEvent eb MonitorTransactions $ \ev -> do
  -- fetch the list of transactions from the wallet
  -- TODO: fetch only the transactions that are not in the database
  -- or starting from the first pending transaction
  transactions <-  getTransactionList wc >>= handleResponse
  addField ev $ TransactionsCount $ length transactions
  synchronizeDbTransactions transactions
  activateSubscriptions (subEventBackend ev)
  -- synchronize wallets
  isOurAddress <- getIsOurAddress
  liftIO (readIORef refAssignments)
    >>= resyncWallets (narrowEventBackend InjectProfileWalletSync eb) wc isOurAddress minAssignmentAmount
    >>= liftIO . writeIORef refAssignments

  certifyRuns (subEventBackend ev) wc
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
      void $ forever $ do
        updateAdaPrice (subEventBackend ev) adaPriceRef
        monitorWalletTransactions (subEventBackend ev) args minAssignmentAmount ref
        liftIO $ threadDelay delayInMicroseconds
    delayInMicroseconds = delayInSeconds * 1000000

updateAdaPrice :: (MonadIO m,MonadMask m)
               => EventBackend m r SynchronizerSelector
               -> IORef (Maybe DB.AdaUsdPrice)
               -> m ()
updateAdaPrice eb ref = withEvent eb UpdateAdaPrice $ \_ -> do
  -- fetch the ada price from the wallet
  adaPrice <- getAdaPrice ( narrowEventBackend InjectCoinGeckoClient eb )
  liftIO $ writeIORef ref $
    case adaPrice of
     Left _ -> Nothing
     Right p -> Just p

