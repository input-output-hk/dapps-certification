{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE OverloadedRecordDot  #-}
module Plutus.Certification.ProfileWallet
  ( resyncWallets
  , renderProfileWalletSyncSelector
  , ProfileWalletSyncSelector
  , PrevAssignments
  , AddressReservation(..)
  , getTemporarilyWalletAddress
  , AddressRotation(..)
  , emptyAddressRotation
  , WalletAddress(..)
  , ProfileAddress(..)
  ) where

import Prelude as P

import Servant.Client.Core
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Control.Monad as M
import Control.Lens.Internal.CTypes (Word64)
import Data.Text (Text)
import Conduit (MonadIO)
import Control.Arrow
import Data.Functor
import Control.Monad.Catch (MonadMask)
import Data.Map (Map)
import Plutus.Certification.Internal
import Data.Aeson.QQ
import Plutus.Certification.WalletClient (WalletArgs)
import Data.Either
import Observe.Event.Render.JSON
import Observe.Event
import Data.Function

import qualified Plutus.Certification.WalletClient as Wallet
import qualified IOHK.Certification.Persistence as DB
import qualified IOHK.Certification.SignatureVerification as Sig
import qualified Data.Vector as V
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- | Basic types for the profile wallet

newtype WalletAddress = WalletAddress { unWalletAddress :: Text }
                      deriving (Eq,Show,Ord)

newtype ProfileAddress = ProfileAddress { unProfileAddress :: Text }
                      deriving (Ord,Eq,Show)

data AddressReservation = Reserved | Overlapping

data ProfileWallet = ProfileWallet
    { pwAddressReservation :: (AddressReservation, WalletAddress)
    , pwProfileAddress :: ProfileAddress
    , pwBalance :: Word64
    }

data Transaction = SimplePayment WalletAddress Word64
                 | DesignatedPayment ProfileAddress WalletAddress Word64
                 | WalletAddressAssignment ProfileAddress WalletAddress

type ProfileWallets = [ProfileWallet]
type ExtraMoney = Word64

newtype PayerMetadata = PayerMetadata { unPayerMetadata :: Text }
                      deriving (Eq,Show)
-- it will look like this:
--
-- {
--   "0": {
--     "map": [
--       {
--         "k": {
--           "string": "payer"
--         },
--         "v": {
--           "list": [
--             {
--               "string":"addr_test1qphgqts20fhx0yx7ug42xehcnryukchy5k7hpaksgxax2fzt5w2gu3"
--             },
--             {
--               "string": "3s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asptcrtp"
--             }
--           ]
--         }
--       }
--     ]
--   }
-- }

instance FromJSON PayerMetadata where
  parseJSON = fmap PayerMetadata . parseMetaData "PayerMetadata" "payer"

--           \"list\": [ { \"string\":\"addr_test1qphgqts20fhx0yx7ug42xehcnryukchy5k7hpaksgxax2fzt5w2gu3\" }, { \"string\": \"3s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asptcrtp\" } ]

{-
>>> t = "{ \"0\": { \"map\": [ { \"k\": { \"string\": \"payer\" }, \"v\": {\"list\": [ { \"string\":\"addr_test1qphgqts20fhx0yx7ug42xehcnryukchy5k7hpaksgxax2fzt5w2gu3\" }, { \"string\": \"3s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asptcrtp\" } ]} } ] } }"
>>> eitherDecode t :: Either String PayerMetadata
Right (PayerMetadata {unPayerMetadata = "addr_test1qphgqts20fhx0yx7ug42xehcnryukchy5k7hpaksgxax2fzt5w2gu33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asptcrtp"})
-}

newtype AddressAssignmentMetadata = AddressAssignmentMetadata  { unAddressAssignmentMetadata  :: Text }
                          deriving (Eq,Show)

instance FromJSON AddressAssignmentMetadata  where
  parseJSON = fmap AddressAssignmentMetadata . parseMetaData "AddressAssignment" "assignment"

{-
>>> t = "{ \"0\": { \"map\": [ { \"k\": { \"string\": \"assignment\" }, \"v\": {\"list\": [ { \"string\":\"addr_test1qphgqts20fhx0yx7ug42xehcnryukchy5k7hpaksgxax2fzt5w2gu3\" }, { \"string\": \"3s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asptcrtp\" } ]} } ] } }"
>>> eitherDecode t :: Either String AddressAssignmentMetadata
Right (AddressAssignmentMetadata {unAddressAssignmentMetadata = "addr_test1qphgqts20fhx0yx7ug42xehcnryukchy5k7hpaksgxax2fzt5w2gu33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asptcrtp"})
-}

parseMetaData :: String -> Text -> Value -> Parser Text
parseMetaData label keyName = withObject label $ \o -> do
    (Array ar) <- o .: "0" >>= (.: "map")
    -- vector to list
    -- and map to (key, value)
    xs <- M.mapM extractKeys (V.toList ar)
    case xs of
      [(key,payerAddress)] | keyName == key -> return payerAddress
      _ -> fail $ "Failed reading: map is not [(<"++ show keyName P.++">,<address>)]"
  where
  extractKeys (Object o') = do
    (String key) <- (o' .: "k") >>= (.: "string")
    (Array ar) <- (o' .: "v") >>= (.: "list")
    -- map vector of Value to vector of String
    value <- mconcat . V.toList <$> V.mapM extractString ar
    return (key, value)
  extractKeys _ = fail "Failed reading: map is not an object"
  extractString = withObject "extractString" $ \o -> do
    (String s) <- o .: "string"
    return s

--------------------------------------------------------------------------------
-- | Events

data ResyncWalletsArgs = TransactionMappingErrors !Int
                       | ExtraMoney !ExtraMoney
                       | TransactionMappingCounts !(Int,Int,Int)

data AssignAddressArgs
  = AssignAddressInputs !(ProfileAddress, WalletAddress, OverlappingAddress)
  | AssignAddressError !String
  | AssignAddressTxResp !Wallet.TxResponse

data ProfileWalletSyncSelector f where
  ResyncWallets :: ProfileWalletSyncSelector ResyncWalletsArgs
  UpdateDbWallet :: ProfileWalletSyncSelector DB.ProfileWallet
  AssignAddress :: ProfileWalletSyncSelector AssignAddressArgs
  UnassignAddressListError :: ProfileWalletSyncSelector ClientError

renderProfileWalletSyncSelector :: RenderSelectorJSON ProfileWalletSyncSelector
renderProfileWalletSyncSelector UpdateDbWallet =
  ("update-db-wallet", renderDbProfileWallet)
renderProfileWalletSyncSelector ResyncWallets =
  ("resync-wallets", \case
    TransactionMappingErrors errors -> ("transaction-mapping-errors", toJSON errors)
    ExtraMoney extraMoney -> ("extra-money", toJSON extraMoney)
    TransactionMappingCounts (simple,designated,assignments) ->
      ("transaction-mapping-count", toJSON [aesonQQ|
          { "simplePayments" : #{simple}
          , "designatedPayments" : #{designated}
          , "addressAssignments" : #{assignments}
          } |]
      )
  )
renderProfileWalletSyncSelector AssignAddress = ("assign-address", \case
  AssignAddressInputs (ProfileAddress{..}, WalletAddress{..}, overlappingAddress) ->
    let overlappingAddress' = overlappingAddress.unWalletAddress
    in ("assign-address-inputs", toJSON [aesonQQ|
      { "profileAddress" : #{unProfileAddress}
      , "destinationAddress" : #{unWalletAddress}
      , "overlappingAddress" : #{overlappingAddress'}
      } |] )
  AssignAddressError err -> ("assign-address-error", toJSON err)
  AssignAddressTxResp txResp -> ("assign-address-tx-id", toJSON txResp)
  )
renderProfileWalletSyncSelector UnassignAddressListError
  = ("unassign-address-list-error", \err -> ("error", toJSON (show err)))

renderDbProfileWallet :: RenderFieldJSON DB.ProfileWallet
renderDbProfileWallet wallet = ("profile-wallet", toJSON wallet)

type WalletBackend m r = EventBackend m r ProfileWalletSyncSelector

--------------------------------------------------------------------------------
-- | MAIN WALLET SYNC FUNCTIONS

resyncWallets :: (MonadIO m,MonadMask m)
              => WalletBackend m r
              -> WalletArgs
              -> PrevAssignments
              -> m PrevAssignments
resyncWallets eb wargs prevAssignments = withEvent eb ResyncWallets \ev -> do
  -- fetch the db transactions and create wallets
  (errors,trans) <- DB.withDb ( DB.getAllTransactions False )
    <&> (lefts &&& rights) . fmap fromDbTransaction'
  addField ev $ TransactionMappingErrors (length errors)
  addField ev $ TransactionMappingCounts (countTransactions trans)

  let (profileWallets,extraMoney) = createProfileWallets trans
  addField ev $ ExtraMoney extraMoney

  -- sync the wallets with the db
  syncDbProfileWallets eb profileWallets

  -- assign addresses for overlapping wallets
  reassignOverlappingAddresses eb wargs profileWallets prevAssignments

  where
  fromDbTransaction' = uncurry (fromDbTransaction isOurAddress)
  mainHash = hash wargs.walletAddress
  hash = Sig.bech32AddressHash . Sig.Bech32Address
  isOurAddress = (== mainHash) . hash
  countTransactions = foldl' (\(a,b,c) -> \case
    SimplePayment{} -> (a+1,b,c)
    DesignatedPayment{} -> (a,b+1,c)
    WalletAddressAssignment{} -> (a,b,c+1)
    ) (0,0,0)
--------------------------------------------------------------------------------
-- | ADDRESS ASSIGNMENT DISSEMINATION

data Assignment = Assignment
                { assgnOverlappingAddress :: WalletAddress
                , assgnDestinationAddress :: WalletAddress
                , assgnProfileAddress :: ProfileAddress
                } deriving (Eq,Show)
type PrevAssignments = [Assignment]

type OverlappingAddress = WalletAddress
type DestinationAddress = WalletAddress


-- TODO: before calling this verify profileId is not a wallet address
assignAddress :: (MonadIO m,MonadMask m)
              => WalletBackend m r
              -> WalletArgs
              -> PrevAssignments
              -> ProfileAddress
              -> OverlappingAddress
              -> DestinationAddress
              -> m (Either String PrevAssignments)
assignAddress eb wargs prevAssignments profileAddress overlappingAddress destinationAddress =
  withEvent eb AssignAddress \ev -> do

  addField ev $ AssignAddressInputs (profileAddress, destinationAddress, overlappingAddress )

  if wasAssignedBefore
    then do
      let err =  "Address already assigned"
      addField ev $ AssignAddressError err
      pure $ Left err
    else reserveAddress ev

  where
  reserveAddress ev = do
    let address = split64 (destinationAddress.unWalletAddress)
    let metadata = [aesonQQ| { "assignment" : #{address} } |]

    resp <- Wallet.broadcastTransaction wargs (Just profileAddress.unProfileAddress) metadata
    case resp of
      Left err -> do
        addField ev $ AssignAddressError (show err)
        return $ Left $ show err
      Right resp' -> do
        addField ev $ AssignAddressTxResp resp'
        let assignment = Assignment overlappingAddress destinationAddress profileAddress
        return $ Right (assignment:prevAssignments)

  wasAssignedBefore :: Bool
  wasAssignedBefore =
    -- search to see if the address was already assigned
    any (\Assignment{..} -> assgnOverlappingAddress == overlappingAddress
                         && assgnProfileAddress == profileAddress
    ) prevAssignments

reassignOverlappingAddresses :: forall m r. (MonadIO m,MonadMask m)
                             => WalletBackend m r
                             -> WalletArgs
                             -> ProfileWallets
                             -> PrevAssignments
                             -> m PrevAssignments
reassignOverlappingAddresses eb wargs profileWallets prevAssignments = do
  -- filter out non overlapping addresses or addresses that were already assigned
  let overlappingAddress = filter isOverlappingAddress profileWallets
  -- get unused addresses which were not assigned before
  unusedAddresses <- (fmap . filter) (not . wasAssignedBefore) <$>
    Wallet.getWalletAddresses wargs (Just Wallet.Unused)
  case unusedAddresses of
    Left err -> withEvent eb  UnassignAddressListError \ev -> do
      addField ev err
      return prevAssignments
    Right unusedAddresses' ->
      -- zip overlapping addresses with unused addresses
      let assignments = zip overlappingAddress unusedAddresses'
      in foldM assignAddress' prevAssignments assignments
  where
  assignAddress' :: (MonadIO m)
                 => PrevAssignments
                 -> (ProfileWallet, Wallet.WalletAddressInfo)
                 -> m PrevAssignments
  assignAddress' prevAssignments' (ProfileWallet{..},addressInfo) = do
    prevAddressE <- assignAddress eb wargs prevAssignments' pwProfileAddress
      (snd pwAddressReservation) (WalletAddress addressInfo.addressId)
    case prevAddressE of
      Left _ -> return prevAssignments
      Right prevAddress -> return prevAddress
  isOverlappingAddress :: ProfileWallet -> Bool
  isOverlappingAddress (ProfileWallet (Overlapping,address) profileAddress _) =
    let isAlreadyAssigned =
          any (\Assignment{..} -> assgnOverlappingAddress == address
                               && assgnProfileAddress == profileAddress
              ) prevAssignments
    in not isAlreadyAssigned
  isOverlappingAddress _ = False
  wasAssignedBefore :: Wallet.WalletAddressInfo -> Bool
  wasAssignedBefore info = any (\Assignment{..} ->
    assgnDestinationAddress == WalletAddress info.addressId) prevAssignments

--------------------------------------------------------------------------------
--  DB RELATED FUNCTIONS

-- | Transform a transaction coming from the database into a `Transaction`
fromDbTransaction :: (Text -> Bool)
                  -> DB.MinimalTransaction
                  -> [DB.MinimalTransactionEntry]
                  -> Either String Transaction
fromDbTransaction isOurAddress DB.MinimalTransaction{..} entries =
  case (paymentMetadata,addressAssignmentMetadata,firstOutWalletAddress) of
    -- address assignment
    (_,Right (AddressAssignmentMetadata address),Just walletAddress)
      | mtxAmount < 0 && not (isOurAddress address)
        -> Right $ WalletAddressAssignment (ProfileAddress address) walletAddress
      | isOurAddress address -> Left "Address assignment should belong to our wallet"
      | otherwise -> Left "Address assignment transaction must be a withdrawal"

    (_,Right (AddressAssignmentMetadata _),Nothing) ->
      Left "InternalError: no wallet address found for address assignment"

    (Right (PayerMetadata address),_,Just walletAddress)
      | mtxAmount > 0 && not (isOurAddress address)
        -> Right $ DesignatedPayment (ProfileAddress address)
            walletAddress (fromIntegral mtxAmount)
      | isOurAddress address ->
        Left "Payer address should not belong to our wallet"

    (_,_,Just walletAddress)
      | mtxAmount > 0 -> Right $ SimplePayment walletAddress (fromIntegral mtxAmount)
      | mtxAmount < 0 -> Left "Payment transaction is an outgoing transaction without address assignment"
    _ -> Left "Transaction is not a standard payment"
  where
  firstOutWalletAddress :: Maybe WalletAddress
  firstOutWalletAddress =
    -- from the list of entries, find the first entry that is an output
    -- and that is in our wallet
    let filtered = filter condition entries
        condition DB.MinimalTransactionEntry{..} =
          not mteInput && isOurAddress mteAddress
    in case filtered of
      [] -> Nothing
      (DB.MinimalTransactionEntry{..}:_) -> Just (WalletAddress mteAddress)

  paymentMetadata :: Either String PayerMetadata
  paymentMetadata = eitherDecodeStrict' (T.encodeUtf8 mtxMetadata)

  addressAssignmentMetadata :: Either String AddressAssignmentMetadata
  addressAssignmentMetadata = eitherDecodeStrict' (T.encodeUtf8 mtxMetadata)

-- | Get the list of all the wallets in the database
--  and update the database with the new list of wallets
syncDbProfileWallets :: forall m r. (MonadIO m, MonadMask m)
                     => WalletBackend m r
                     ->  ProfileWallets
                     -> m ()
syncDbProfileWallets eb wallets = do
    DB.withDb DB.getProfileWallets >>= mapM_ updateProfileWallet
  where
  pwMap :: Map ProfileAddress ProfileWallet
  pwMap = Map.fromList (map (\wallet@(ProfileWallet _ address _) -> (address,wallet)) wallets)

  updateProfileWallet :: (MonadIO m,MonadMask m) => (DB.Profile, Maybe DB.ProfileWallet) -> m ()
  updateProfileWallet (DB.Profile{..},dbWalletM) =
    case (Map.lookup (ProfileAddress ownerAddress) pwMap,dbWalletM) of
      -- if there is no wallet in the db, create it
      (Just wallet,Nothing) -> upsertWallet (profileWalletToDBProfileWallet profileId wallet)
      -- if there is a wallet in the db
      -- and it is different from the one in the map update it
      (Just wallet,Just dbProfileWallet)
        | dbProfileWallet' <- profileWalletToDBProfileWallet profileId wallet
        , dbProfileWallet /= dbProfileWallet' -> upsertWallet dbProfileWallet'
      -- do nothing if there is no wallet in the map or the wallet is the same
      _ -> pure ()

  upsertWallet wallet = withEvent eb UpdateDbWallet \ ev -> do
    DB.withDb $ DB.upsertProfileWallet wallet
    addField ev wallet

  profileWalletToDBProfileWallet :: DB.ProfileId -> ProfileWallet -> DB.ProfileWallet
  profileWalletToDBProfileWallet profileId ProfileWallet{..} =
    DB.ProfileWallet
      { profileWalletId = profileId
      , profileWalletAddress = unWalletAddress $ snd pwAddressReservation
      , profileWalletCredits = fromIntegral pwBalance
      , profileWalletStatus = case fst pwAddressReservation of
          Reserved -> DB.Reserved
          Overlapping -> DB.Overlapping
      }
--------------------------------------------------------------------------------
-- | ADDRESS ROTATION

data AddressRotation = AddressRotation
  { byAddress :: Map WalletAddress [DB.ProfileId]
  , byProfile :: Map DB.ProfileId WalletAddress
  } deriving (Show,Eq)

emptyAddressRotation :: AddressRotation
emptyAddressRotation = AddressRotation
  { byAddress = Map.empty
  , byProfile = Map.empty
  }

getTemporarilyWalletAddress :: [WalletAddress]
                            -> DB.ProfileId
                            -> AddressRotation
                            ->  (Maybe WalletAddress,AddressRotation)
getTemporarilyWalletAddress addresses profileId rotation =
  case (profileAddress,nextAddress) of
    (Just address,_) -> (Just address,rotation)
    (Nothing,Just address) -> (Just address,AddressRotation
      { byAddress = Map.insertWith (++) address [profileId] (byAddress rotation)
      , byProfile = Map.insert profileId address (byProfile rotation)
      })
    (Nothing,Nothing) -> (Nothing,rotation)

  where
  nextAddress :: Maybe WalletAddress
  nextAddress = case sortedAddress of
    [] -> Nothing
    ((address,_):_) -> Just address

  profileAddress :: Maybe WalletAddress
  profileAddress = Map.lookup profileId (byProfile rotation)

  sortedAddress :: [(WalletAddress,Int)]
  sortedAddress  = sortBy (compare `on` snd) $
    map (\address -> (address,Map.findWithDefault 0 address byAddressWithWeight)) addresses

  byAddressWithWeight :: Map WalletAddress Int
  byAddressWithWeight = Map.fromListWith (+) (map (second length) (Map.toList (byAddress rotation)))

--------------------------------------------------------------------------------
-- | ProfileWallets related functions

createProfileWallets :: [Transaction] -> (ProfileWallets,ExtraMoney)
createProfileWallets = P.foldl updateWallets ([],0)
  where
  updateWallets :: (ProfileWallets,ExtraMoney) -> Transaction -> (ProfileWallets,ExtraMoney)
  updateWallets (wallets,extraMoney) transaction =
    let (updatedWallets,extraMoney') = updateProfileWallets wallets transaction
    in (updatedWallets,extraMoney + extraMoney')

updateProfileWallets :: ProfileWallets -> Transaction -> (ProfileWallets,ExtraMoney)
updateProfileWallets wallets (SimplePayment receiverAddress amount) =
  -- check if there is a reserved wallet for the receiverAddress
  -- and if so, update the balance of the wallet
  -- otherwise, return the wallets and the amount as extra money
  case updateBalanceByWalletAddress wallets receiverAddress amount of
    (updatedWallets,True) -> (updatedWallets,0)
    (updatedWallets,False) -> (updatedWallets,amount)

updateProfileWallets wallets (DesignatedPayment profileAddress receiverAddress amount) =
  -- we can't have any extra money, because we know whose wallet we need to update
  -- also here we reserve the receiverAddress if it is not already reserved
  -- otherwise we mark it as overlapping
  (searchAndUpdate False wallets,0)
  where
  -- when the profileAddress is already reserved, we need to:
  searchAndUpdate addressReserved (wallet@(ProfileWallet (Reserved,address) profileAddress' balance):wallets')
    -- check if the profileAddress is the same and if so, we need to update the balance
    -- NOTE: we don't care about `receiverAddress` specified in the transaction because
    -- we already have a reserved address
    | profileAddress' == profileAddress = (wallet { pwBalance = balance + amount }):wallets'
    -- otherwise we continue searching
    -- and mark `receiverAddress` as reserved if it the same as the `address`
    | otherwise = wallet:searchAndUpdate (addressReserved || address == receiverAddress) wallets'

  -- when the profileAddress is not reserved, but overlapping
  searchAndUpdate addressReserved (wallet@(ProfileWallet (Overlapping,_) profileAddress' balance):wallets')
    -- we need to check if the profileAddress is the same and if so, update the balance
    | profileAddress' == profileAddress = (wallet
        { pwBalance = balance + amount
        -- but also we need to update the address reservation
        , pwAddressReservation = (boolToReservation addressReserved, receiverAddress)
        }):wallets'
    -- otherwise we continue searching
    | otherwise = wallet:searchAndUpdate addressReserved wallets'

  -- if we reach the empty list, we need to create a new profile wallet
  searchAndUpdate addressReserved [] =
    [ProfileWallet (boolToReservation addressReserved, receiverAddress) profileAddress amount]

updateProfileWallets wallets (WalletAddressAssignment profileAddress receiverAddress) =
  -- we can't have any extra money, because we just reserve the address
  (reserveAddress wallets,0)
  where
  -- when the profileAddress is already reserved, we need to:
  reserveAddress (wallet@(ProfileWallet (Reserved,address) profileAddress' _):wallets')
    -- change the address if there is forced address assignation
    -- for the same profileAddress
    | profileAddress' == profileAddress = (wallet
        { pwAddressReservation = (Reserved, receiverAddress) }):wallets'
    -- or, if there is different profile with the pointing to the same receiverAddress
    -- we ignore the assignment because the address is already reserved
    | address == receiverAddress = wallet:wallets'
    -- otherwise we continue searching
    | otherwise = wallet:reserveAddress wallets'

  -- when the profileAddress is not reserved, but overlapping
  reserveAddress (wallet@(ProfileWallet (Overlapping,_) profileAddress' _):wallets')
    -- we need to check if the profileAddress is the same and if so,
    -- update the address reservation
    | profileAddress' == profileAddress = (wallet
        { pwAddressReservation = (Reserved, receiverAddress) }):wallets'
    -- otherwise we continue searching
    | otherwise = wallet:reserveAddress wallets'
  -- if we reach the empty list, we need to create a new profile wallet
  -- with the reserved address and the balance of 0
  reserveAddress [] = [ProfileWallet (Reserved, receiverAddress) profileAddress 0]

boolToReservation :: Bool -> AddressReservation
boolToReservation True = Overlapping
boolToReservation False = Reserved

updateBalanceByWalletAddress :: [ProfileWallet] -> WalletAddress -> Word64 -> ([ProfileWallet],Bool)
updateBalanceByWalletAddress (wallet@(ProfileWallet (Reserved, walletAddress) _ balance) : rest) walletAddress' amount
    | walletAddress == walletAddress' = (wallet { pwBalance = balance + amount } : rest,True)
updateBalanceByWalletAddress (wallet:rest) walletAddress' amount =
  let (updatedWallets,found) = updateBalanceByWalletAddress rest walletAddress' amount
  in (wallet : updatedWallets,found)
updateBalanceByWalletAddress [] _ _ = ([],False)
