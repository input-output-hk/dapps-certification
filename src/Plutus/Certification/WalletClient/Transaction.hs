{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Plutus.Certification.WalletClient.Transaction where
import           Data.Aeson
import           Data.Text         hiding (drop, toLower)
import           GHC.Generics

import           Control.Monad
import           Data.Proxy
import           Data.Time
import           GHC.TypeLits

import qualified Data.Aeson.KeyMap as KM

newtype JSONCustomOptions n a = JSONCustomOptions a deriving Generic

defaultRecordTypeOptions :: Int -> Options
defaultRecordTypeOptions n = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop n
  , constructorTagModifier = camelTo2 '_' . drop n
  }

instance (Generic a, GToJSON' Value Zero (Rep a),KnownNat n) => ToJSON (JSONCustomOptions n a)
    where
    toJSON (JSONCustomOptions x) = genericToJSON (defaultRecordTypeOptions (nToDrop @n)) x

nToDrop :: forall n. KnownNat n => Int
nToDrop = fromInteger $ natVal (Proxy :: Proxy n)

instance (Generic a, GFromJSON Zero (Rep a), KnownNat n) => FromJSON (JSONCustomOptions n a)
  where
  parseJSON = fmap JSONCustomOptions . genericParseJSON (defaultRecordTypeOptions (nToDrop @n))

newtype Quantity unit = Quantity { quantity :: Int }
                   deriving (Show,Eq,Generic)
type LovelaceQty = Quantity "lovelace"
type BlockQty = Quantity "block"

instance (KnownSymbol unit) => ToJSON (Quantity unit) where
  toJSON Quantity{..} = object
    [ "quantity" .= quantity
    , "unit" .= symbolVal (Proxy :: Proxy unit)
    ]

instance (KnownSymbol unit) => FromJSON (Quantity unit) where
  parseJSON = withObject strType handler
    where
    label = symbolVal (Proxy :: Proxy unit)
    strType = "Quantity<"++ label ++">"
    handler v = do
      unit <- v .: "unit"
      unless (unit == label) $ fail $ "unit property must be equal to " ++ label
      Quantity <$> v .: "quantity"

-- >>> encode (Quantity @"lovelace" 3)
-- "{\"quantity\":3,\"unit\":\"lovelace\"}"

-- >>> (eitherDecode  "{\"quantity\":3,\"unit\":\"lovelace\"}" :: Either String (Quantity "lovelace"))
-- Right (Quantity {quantity = 3})

-- >>> (eitherDecode  "{\"quantity\":3,\"unit\":\"wrong-unit\"}" :: Either String (Quantity "lovelace"))
-- Left "Error in $: unit property must be equal to lovelace"

data SlotReference = SlotReference
    { absoluteSlotNumber :: !Int
    , slotNumber         :: !Int
    , epochNumber        :: !Int
    , time               :: !UTCTime
    }
    deriving (Eq, Show, Generic)
    deriving (FromJSON,ToJSON) via (JSONCustomOptions 0 SlotReference)

-- >>> encode $ SlotReference 1 2 3 (read "2022-01-01 12:00:00")
-- "{\"absolute_slot_number\":1,\"epoch_number\":3,\"slot_number\":2,\"time\":\"2022-01-01T12:00:00Z\"}"

-- >>> (eitherDecode "{\"absolute_slot_number\":1,\"epoch_number\":3,\"slot_number\":2,\"time\":\"2022-01-01T12:00:00Z\"}" :: Either String SlotReference)
-- Right (SlotReference {absoluteSlotNumber = 1, slotNumber = 2, epochNumber = 3, time = 2022-01-01 12:00:00 UTC})

data BlockReference = BlockReference
    { apiSlotReference :: !SlotReference
    , height           :: !BlockQty
    }
    deriving (Eq,Show,Generic)

instance ToJSON BlockReference where
  toJSON (BlockReference slotRef h) = Object (x <> y)
    where
    x = case toJSON slotRef of
            Object obj -> obj
            _          -> KM.empty
    y = KM.fromList [ "height" .= h]

instance FromJSON BlockReference where
  parseJSON = withObject "BlockReference" handler
    where
    handler v = do
      BlockReference
        <$> parseJSON (Object v)
        <*> v .: "height"

-- >>> encode $ BlockReference (SlotReference 1 2 3 (read "2022-01-01 12:00:00")) (Quantity 4)
-- "{\"absolute_slot_number\":1,\"epoch_number\":3,\"height\":{\"quantity\":4,\"unit\":\"block\"},\"slot_number\":2,\"time\":\"2022-01-01T12:00:00Z\"}"

-- >>> (eitherDecode "{\"absolute_slot_number\":1,\"epoch_number\":3,\"height\":{\"quantity\":4,\"unit\":\"block\"},\"slot_number\":2,\"time\":\"2022-01-01T12:00:00Z\"}":: Either String BlockReference)
-- Right (BlockReference {apiSlotReference = SlotReference {absoluteSlotNumber = 1, slotNumber = 2, epochNumber = 3, time = 2022-01-01 12:00:00 UTC}, height = Quantity {quantity = 4}})

data Direction = Incoming | Outgoing
               deriving (Eq,Show,Generic)
               deriving (ToJSON,FromJSON) via (JSONCustomOptions 0 Direction)

-- >>> toJSON Incoming
-- String "incoming"

-- >>> (eitherDecode "\"incoming\"" :: Either String Direction)
-- Right Incoming

newtype PublicAddress = PublicAddress { unPublicAddress :: Text }
                      deriving (Eq,Show,ToJSON,FromJSON)

data TxOutput = TxOutput
  { txOutputAddress :: !PublicAddress
  , txOutputAmount  :: !LovelaceQty
  } deriving (Eq,Show,Generic)
    deriving (ToJSON,FromJSON) via (JSONCustomOptions 8 TxOutput)

-- >>> encode (TxOutput (PublicAddress "addr1") (Quantity 4))
-- "{\"address\":\"addr1\",\"amount\":{\"quantity\":4,\"unit\":\"lovelace\"}}"

-- >>> (eitherDecode "{\"address\":\"addr1\",\"amount\":{\"quantity\":4,\"unit\":\"lovelace\"}}" :: Either String TxOutput)
-- Right (TxOutput {txOutputAddress = "addr1", txOutputAmount = Quantity {quantity = 4}})

data TxInput = TxInput
  { txInputIndex  :: !Int
  , txInputId     :: !TransactionId
  , txInputSource :: !(Maybe TxOutput)
  } deriving (Eq,Show)

instance ToJSON TxInput where
  toJSON (TxInput{..}) = Object (sourceObject <> restOfObject)
    where
    sourceObject = case txInputSource of
          Just val -> case toJSON val of
            Object obj -> obj
            _          -> KM.empty
          Nothing -> KM.empty
    restOfObject = KM.fromList
      [ "id" .= txInputId
      , "index" .= txInputIndex
      ]
instance FromJSON TxInput where
  parseJSON = withObject "TxInput" handler
    where
    handler v = do
      (addressM :: Maybe PublicAddress) <- v .:? "address"
      (amountM :: Maybe LovelaceQty) <- v .:? "amount"
      txOutput <- case (addressM,amountM) of
        (Nothing,Nothing) -> pure Nothing
        (Just addr,Just amount) -> pure $ Just $ TxOutput addr amount
        (_,_) -> fail "can't set just one of property `address` or `amount`. you do both or none"
      TxInput
        <$> v .: "index"
        <*> v .: "id"
        <*> pure txOutput

-- >>> encode (TxInput 1 "txId" (Just (TxOutput "addr1" (Quantity 4))))
-- "{\"address\":\"addr1\",\"amount\":{\"quantity\":4,\"unit\":\"lovelace\"},\"id\":\"txId\",\"index\":1}"

-- >>> (eitherDecode  "{\"address\":\"addr1\",\"amount\":{\"quantity\":4,\"unit\":\"lovelace\"},\"id\":\"txId\",\"index\":1}":: Either String TxInput)
-- Right (TxInput {txInputIndex = 1, txInputId = "txId", txInputSource = Just (TxOutput {txOutputAddress = "addr1", txOutputAmount = Quantity {quantity = 4}})})

data TxPending
data TxSubmitted
data TxInLedger
data TxExpired

data StatusWithData a where
  InLedger :: BlockReference -> StatusWithData TxInLedger
  Submitted :: StatusWithData TxSubmitted
  -- the TxExpired represents a future state when then transaction will expire
  -- NOTE: we don't know if that value exists always when the transaction is in pending state
  Pending :: BlockReference -> Maybe (StatusWithData TxExpired) -> StatusWithData TxPending
  Expired :: BlockReference -> StatusWithData TxExpired



instance FromJSON GenericStatusWithData where
  parseJSON = withObject "GenericStatusWithData" handler
    where
    handler v = do
      (status :: Text) <- v .: "status"
      case status of
        "pending" -> do
          (expiresAt :: Maybe BlockReference) <- v .:? "expires_at"
          (pendingSince :: BlockReference) <- v .: "pending_since"
          pure (GenericStatusWithData $ Pending pendingSince (Expired <$> expiresAt))
        "submitted" -> pure (GenericStatusWithData Submitted)
        "in_ledger" -> GenericStatusWithData . InLedger <$> v .: "inserted_at"
        "expired" -> GenericStatusWithData . Expired <$> v .: "block"
        _ -> fail "unknown status"

instance ToJSON GenericStatusWithData where
  toJSON (GenericStatusWithData status) = case status of
    Pending pendingSince expiresAt -> object
      [ "status" .= String "pending"
      , "pending_since" .= pendingSince
      , "expires_at" .= case expiresAt of
          Just (Expired block) -> Just block
          _ -> Nothing
      ]
    Submitted -> object
      [ "status" .= String "submitted"
      ]
    InLedger insertedAt -> object
      [ "status" .= String "in_ledger"
      , "inserted_at" .= insertedAt
      ]
    Expired block -> object
      [ "status" .= String "expired"
      , "block" .= block
      ]


deriving instance Show (StatusWithData a)

instance Eq (StatusWithData a) where
  (Pending a1 b1) == (Pending a2 b2) = a1 == a2 && b1 == b2
  Submitted == Submitted = True
  (InLedger a) == (InLedger b) = a == b
  (Expired a) == (Expired b) = a == b

data GenericStatusWithData = forall a. GenericStatusWithData { unGenericStatusWithData :: StatusWithData a }

instance Eq GenericStatusWithData where
  (GenericStatusWithData a) == (GenericStatusWithData b) =
    case (a,b) of
      (Pending _ _, Pending _ _) -> a == b
      (InLedger _ , InLedger _) -> a == b
      (Expired _, Expired _) -> a == b
      (Submitted, Submitted) -> True
      _ -> False


-- >>> encode TxInLedger
-- "\"in_ledger\""
--
-- >>> (eitherDecode  "\"in_ledger\"":: Either String TxStatus)
-- Right TxInLedger

-- >>> encode TxPending
-- "\"pending\""
-- >>> (eitherDecode  "\"pending\"":: Either String TxStatus)
-- Right TxPending

newtype TransactionId = TransactionId { txId :: Text }
                      deriving (Show,Eq,ToJSON,FromJSON)

data WalletTransactionData = WalletTransactionData
    { walletTxId              :: !TransactionId
    , walletTxAmount          :: !LovelaceQty
    , walletTxFee             :: !LovelaceQty
    , walletTxDepositTaken    :: !LovelaceQty
    , walletTxDepositReturned :: !LovelaceQty
    , walletTxDepth           :: !(Maybe BlockQty)
    , walletTxDirection       :: !Direction
    , walletTxInputs          :: ![TxInput]
    , walletTxOutputs         :: ![TxOutput]
    , walletTxMetadata        :: !(Maybe Value)
    }
    deriving (Eq, Show,Generic)
    deriving (ToJSON,FromJSON) via (JSONCustomOptions 8 WalletTransactionData)

data WalletTransaction = forall a. WalletTransaction
    { walletTxData            :: !WalletTransactionData
    , walletTxStatus          :: !(StatusWithData a)
    }

instance Eq WalletTransaction where
  (WalletTransaction d1 s1) == (WalletTransaction d2 s2) = d1 == d2
    && (GenericStatusWithData s1 == GenericStatusWithData s2)

instance Show WalletTransaction where
  show (WalletTransaction d s) = "WalletTransaction " <> show d <> " " <> show s

instance FromJSON WalletTransaction where
  parseJSON = withObject "WalletTransaction" handler
    where
    handler v = do
      txData <- parseJSON (Object v)
      (GenericStatusWithData txStatus) <- parseJSON (Object v)
      pure $ WalletTransaction txData txStatus

instance ToJSON WalletTransaction where
  toJSON (WalletTransaction txData txStatus) = Object (x <> y)
    where
    x = case toJSON txData of
            Object obj -> obj
            _          -> KM.empty
    y = case toJSON (GenericStatusWithData txStatus) of
            Object obj -> obj
            _          -> KM.empty
