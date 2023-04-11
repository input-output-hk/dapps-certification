{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Plutus.Certification.Synchronizer
  ( startTransactionsMonitor
  , SynchronizerSelector(..)
  , renderSynchronizerSelector
  ) where

import Plutus.Certification.WalletClient.Transaction
import Plutus.Certification.WalletClient
import Control.Monad (forever, forM_, void, when)
import Control.Concurrent (threadDelay)
import Data.Time (UTCTime)
import Data.ByteString (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Catch (MonadMask)
import Data.List (groupBy)
import Plutus.Certification.API.Routes (RunIDV1(..))
import Data.Aeson
import Plutus.Certification.CertificationBroadcaster
import Observe.Event.Render.JSON
import Control.Exception

import qualified IOHK.Certification.Persistence as DB

import Data.Function (on)
import Data.UUID (UUID)
import Control.Monad.Except (MonadError)
import Data.Maybe (fromMaybe)
import Observe.Event.Backend
import Observe.Event
import Plutus.Certification.ProfileWallet
import Data.IORef

data InitializingField
  = WalletArgsField WalletArgs
  | DelayField Int

data SynchronizerSelector f where
  InitializingSynchronizer :: SynchronizerSelector InitializingField
  InjectTxBroadcaster :: forall f . !(TxBroadcasterSelector f) -> SynchronizerSelector f
  InjectProfileWalletSync :: forall f . !(ProfileWalletSyncSelector f) -> SynchronizerSelector f
  MonitorTransactions :: SynchronizerSelector TransactionsCount

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
  )
renderSynchronizerSelector (InjectTxBroadcaster selector) = renderTxBroadcasterSelector selector
renderSynchronizerSelector MonitorTransactions = ("monitor-transactions", renderTransactionsCount)
renderSynchronizerSelector (InjectProfileWalletSync selector) = renderProfileWalletSyncSelector selector

renderTransactionsCount :: RenderFieldJSON TransactionsCount
renderTransactionsCount (TransactionsCount count) = ("transactions-count",toJSON count)

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

synchronizeDbTransactions :: (MonadIO m, MonadMask m) => [WalletTransaction] -> m ()
synchronizeDbTransactions transactions = do
  -- filter out the outgoing transactions and sync them with the database
  DB.withDb $ forM_ transactions storeTransaction
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
fromInputToDbInput (TxInput index _ (Just TxOutput{..})) = Just $ DB.TransactionEntry
  { DB.txEntryId = undefined
  , DB.txEntryTxId = undefined
  , DB.txEntryAddress = txOutputAddress.unPublicAddress
  --TODO: remove conversion after merging with feat/subscription
  , DB.txEntryAmount = fromIntegral txOutputAmount.quantity
  , DB.txEntryIndex = Just index
  , DB.txEntryInput = True
  }

monitorWalletTransactions :: (MonadIO m, MonadMask m,MonadError IOException m)
                          => EventBackend m r SynchronizerSelector
                          -> WalletArgs
                          -> IORef PrevAssignments
                          -> m ()
monitorWalletTransactions eb args refAssignments = withEvent eb MonitorTransactions $ \ev -> do
  -- fetch the list of transactions from the wallet
  -- TODO: fetch only the transactions that are not in the database
  -- or starting from the first pending transaction
  transactions <- getTransactionList args >>= handleResponse
  addField ev $ TransactionsCount $ length transactions
  synchronizeDbTransactions transactions
  -- synchronize wallets
  liftIO (readIORef refAssignments)
    >>= resyncWallets (narrowEventBackend InjectProfileWalletSync eb) args
    >>= liftIO . writeIORef refAssignments


  certifyRuns (subEventBackend ev) args
  where
    -- handle the response from the wallet
    -- TODO: crash the server if the connection with the wallet is lost
    handleResponse (Left err) = do
      liftIO $ putStrLn $ "Error while fetching transactions: " ++ show err
      return []
    handleResponse (Right transactions) = return transactions

type CertificationProcess m = DB.ProfileId -> UUID -> m DB.Certification

-- certify all runs who have enough credit to be certified
-- and have not been certified yet
certifyRuns :: (MonadIO m, MonadMask m,MonadError IOException m)
            => EventBackend m r SynchronizerSelector
            -> WalletArgs
            -> m ()
certifyRuns eb args = do
  -- fetch the list of runs from the database
  runs <- DB.withDb DB.getRunsToCertify

  -- group runs by profileId
  let runsByProfile = groupBy ((==) `on` (.profileId)) runs

  -- for each profile runs, certify them
  -- TODO: parallelize this
  forM_ runsByProfile $ certifyProfileRuns certificationProcess
  where
  certificationProcess a b = createCertification
    ( narrowEventBackend InjectTxBroadcaster eb ) args a (RunID b)

-- certify all runs of a profile
certifyProfileRuns :: (MonadIO m, MonadMask m) => CertificationProcess m -> [DB.Run] -> m ()
certifyProfileRuns certificationProcess runs =
  -- get the profile
  DB.withDb (DB.getProfileAddress pid)
    -- and certify the runs
    >>= mapM_ certifyProfileRuns'
  where
  pid = (head runs).profileId
  certifyProfileRuns' address = do
    -- calculate the amount of credits available
    creditsAvailable <- fromMaybe 0 <$> DB.withDb (DB.getProfileBalance address)
    -- recursively certify the runs until we run out of credits
    certifyRuns' runs creditsAvailable

  certifyRuns' [] _ = return ()
  certifyRuns' (run:rs) creditsAvailable = do
    -- calculate the cost of the run
    let cost = fromIntegral run.certificationPrice
    -- if we have enough credits, certify the run
    when (creditsAvailable >= cost) $ void (certificationProcess pid (run.runId))
    -- recursively certify the next runs
    certifyRuns' rs (creditsAvailable - cost)

-- | Start a thread that monitors the wallet transactions
-- and updates the database accordingly.
--
-- The thread will run forever, and will be restarted if it crashes.
-- The delay between each check is specified in seconds.
startTransactionsMonitor :: (MonadIO m,MonadMask m,MonadError IOException m)
                         => EventBackend m r SynchronizerSelector
                         -> WalletArgs
                         -> Int
                         -> m b
startTransactionsMonitor eb args delayInSeconds = withEvent eb InitializingSynchronizer $ \ev -> do
  addField ev $ WalletArgsField args
  addField ev $ DelayField delayInSeconds
  -- TODO maybe a forkIO here will be better than into the calling function
  -- hence, now, the parent instrumentation event will never terminate
  ref <- liftIO $ newIORef []
  forever $ do
    monitorWalletTransactions (subEventBackend ev) args ref
    liftIO $ threadDelay delayInMicroseconds
  where
    delayInMicroseconds = delayInSeconds * 1000000
