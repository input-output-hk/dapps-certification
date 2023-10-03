{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE LambdaCase                 #-}

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IOHK.Certification.Persistence.Structure where

import           Control.Lens         hiding (index, (.=))
import           Data.Aeson
import           Data.Proxy
import           Data.Swagger         hiding (Contact)
import           Database.Selda
import           Database.Selda.SqlType as Selda
import           Control.Exception ( throw)
import           Data.Int (Int64)

import           IOHK.Certification.Persistence.Structure.Profile
import           IOHK.Certification.Persistence.Structure.Subscription
import           IOHK.Certification.Persistence.Pattern
import           Data.Text hiding (index)

import           IOHK.Certification.Interface
  ( GitHubAccessToken(..)
  , ghAccessTokenFromText
  )

import qualified Data.Text         as Text
import qualified Data.Aeson.KeyMap as KM

newtype IpfsCid = IpfsCid { ipfsCid :: Text}
  deriving (ToJSON,FromJSON,Show )

newtype TxId = TxId { txId :: Text}
  deriving (ToJSON,FromJSON,Show,Read)
--------------------------------------------------------------------------------
-- | Subscription
data SubscriptionDTO = SubscriptionDTO
  { subscriptionDtoId :: ID Subscription
  , subscriptionDtoProfileId :: ID Profile
  , subscriptionDtoTierId :: ID Tier
  , subscriptionDtoPrice :: Int64
  , subscriptionDtoAdaUsdPrice :: Double
  , subscriptionDtoStartDate :: UTCTime
  , subscriptionDtoEndDate :: UTCTime
  , subscriptionDtoStatus :: SubscriptionStatus
  , subscriptionDtoName :: Text
  , subscriptionDtoFeatures :: [Feature]
  , subscriptionDtoType :: TierType
  } deriving (Generic)

instance ToJSON SubscriptionDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 15 }

instance FromJSON SubscriptionDTO where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 15 }

instance ToSchema SubscriptionDTO where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions { fieldLabelModifier = dropAndLowerFirst 15 }

data TierDTO = TierDTO
  { tierDtoTier :: Tier
  , tierDtoFeatures :: [Feature]
  }

instance ToJSON TierDTO where
  toJSON TierDTO{..} = Object (x <> y)
    where
    x = case toJSON tierDtoTier of
            Object obj -> obj
            _          -> KM.empty
    y = KM.fromList [ "features" .= tierDtoFeatures ]

instance FromJSON TierDTO where
  parseJSON = withObject "TierDTO" $ \v -> TierDTO
      <$> parseJSON (Object v)
      <*> v .: "features"

instance ToSchema TierDTO where
  declareNamedSchema _ = do
    tierSchema <- declareSchema (Proxy :: Proxy Tier)
    featureSchema <- declareSchemaRef (Proxy :: Proxy [Feature])
    return $ NamedSchema (Just "TierDTO") $ tierSchema
              & properties %~ (`mappend` [ ("features", featureSchema) ])
              & required %~  (<> [ "features" ])

data WalletAddressStatus = Reserved | Overlapping
  deriving (Generic, Show, Eq)

instance FromJSON WalletAddressStatus where
  parseJSON = withText "WalletAddressStatus" $ \case
    "reserved" -> pure Reserved
    "overlapping" -> pure Overlapping
    _ -> fail "WalletAddressStatus must be one of: reserved, overlapping"

instance ToJSON WalletAddressStatus where
  toJSON = \case
     Overlapping -> "overlapping"
     Reserved -> "reserved"

instance ToSchema WalletAddressStatus where
  declareNamedSchema _ = do
    let values = [ "reserved", "overlapping" ] :: [Value]
    return $ NamedSchema (Just "WalletAddressStatus") $ mempty
      & type_ ?~ SwaggerString
      & enum_ ?~ values

instance SqlType WalletAddressStatus where
   mkLit n = LCustom TInt64 (LInt64 (toInt64 n))
     where
     toInt64 = \case
       Overlapping -> 0
       Reserved -> 1
   sqlType _ = TInt64
   fromSql (SqlInt64 0) = Overlapping
   fromSql (SqlInt64 1) = Reserved
   fromSql (SqlInt64 n) = throw $ SqlDataValidationException $ "fromSql: expected 0 or 1, got " ++ show n
   fromSql v            = throw $ userError $ "fromSql: expected SqlInt64, got " ++ show v
   defaultValue = mkLit Overlapping

data ProfileWallet = ProfileWallet
  { profileWalletId :: ID Profile
  , profileWalletAddress :: Text
  , profileWalletStatus :: WalletAddressStatus
  , profileWalletCredits :: Int64
  } deriving (Generic, Show, Eq)

instance SqlRow ProfileWallet

instance ToJSON ProfileWallet where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 14 }

instance FromJSON ProfileWallet where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 14 }

instance ToSchema ProfileWallet where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions { fieldLabelModifier = dropAndLowerFirst 14 }

--------------------------------------------------------------------------------
-- | Profile

instance FromJSON (ID Profile) where
  parseJSON = withText "ID Profile" $ \t -> pure $ toId $ read $ Text.unpack t

instance ToJSON (ID Profile) where
  toJSON = toJSON . show . fromId

newtype DAppDTO = DAppDTO { unDAppDTO :: DApp } deriving (Generic, Show, Eq)

instance ToJSON DAppDTO where
  toJSON (DAppDTO dapp)= case dappGitHubToken dapp of
    Nothing -> toJSON dapp
    Just _  -> Object (KM.insert "githubToken" "<<REDACTED>>" obj)
    where
    obj = case toJSON dapp of
      Object obj' -> obj'
      _ -> error "impossible"

instance FromJSON DAppDTO where
  parseJSON = withObject "DAppDTO" $ \v -> do
    -- remove githubToken from the object
    let v' = KM.delete "githubToken" $ KM.delete "id" v
    DAppDTO <$> parseJSON (Object v')

instance ToSchema DAppDTO where
  declareNamedSchema _ = do
    dappSchema <- declareSchema (Proxy :: Proxy DApp)
    return $ NamedSchema (Just "DAppDTO") dappSchema

data ProfileDTO = ProfileDTO
  { profile :: !Profile
  , dapp    :: !(Maybe DAppDTO)
  } deriving (Show)

-- NOTE: ProfileDTO serialization is not isomorphic
-- because we hide the github access token
instance FromJSON ProfileDTO where
  parseJSON = withObject "ProfileDTO" $ \v -> ProfileDTO
      <$> parseJSON (Object v)
      <*> v .:? "dapp" .!= Nothing

instance ToJSON ProfileDTO where
  toJSON ProfileDTO{..} = object $
    ("dapp" .= dapp) : profileJSONPairs profile

instance ToSchema ProfileDTO where
  declareNamedSchema _ = do
    profileSchema <- declareSchema (Proxy :: Proxy Profile)
    dappSchema <- declareSchemaRef (Proxy :: Proxy DApp)
    return $ NamedSchema (Just "ProfileDTO") $ profileSchema
              & properties %~ (`mappend` [ ("dapp", dappSchema) ])

--------------------------------------------------------------------------------
-- | Dapp

data DApp = DApp
  { dappId      :: ID Profile
  , dappName    :: Text
  , dappOwner   :: Text
  , dappRepo    :: Text
  , dappVersion :: Maybe Text
  , dappGitHubToken :: Maybe GitHubAccessToken
  , dappSubject :: Maybe Subject
  } deriving (Generic,Show,Eq)

instance ToSchema DApp where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    ghTokenSchema <- declareSchemaRef (Proxy :: Proxy GitHubAccessToken)
    return $ NamedSchema (Just "DApp") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("name", textSchema)
          , ("owner", textSchema)
          , ("repo", textSchema)
          , ("version", textSchema)
          , ("githubToken", ghTokenSchema)
          , ("subject", textSchema)
          ]
      & required .~ ["name", "owner", "repo"]

instance FromJSON DApp where
  parseJSON = withObject "DApp" $ \v -> DApp
    <$> (toId <$> v .:? "dappId" .!= (-1))
    <*> v .: "name"
    <*> v .: "owner"
    <*> v .: "repo"
    <*> v .:? "version"
    <*> v .:? "githubToken"
    <*> v .:? "subject"

instance ToJSON DApp where
  toJSON (DApp{..}) = object
      [ "name" .= dappName
      , "owner" .= dappOwner
      , "repo" .= dappRepo
      , "version" .= dappVersion
      , "githubToken" .= dappGitHubToken
      , "subject" .= dappSubject
      ]

instance SqlType GitHubAccessToken where
  mkLit gitHubAccessToken =
    let t = (pack $ show gitHubAccessToken)
     in LCustom TText (LText t)
  sqlType _ = TText
  fromSql (SqlString t)
    | Right token <- ghAccessTokenFromText t = token
    | otherwise = throw $ SqlDataValidationException $ "Invalid GitHubAccessToken: " ++ show t
  fromSql v = throw $ userError $ "fromSql: expected SqlString, got " ++ show v
  defaultValue = throw $ userError "GitHubAccessToken: no default value"

instance SqlRow DApp

--------------------------------------------------------------------------------
-- | Wallet transactions

data TxStatus = Pending | Submitted | InLedger | Expired
              deriving (Show,Read,Eq,Enum,Bounded)

instance SqlEnum TxStatus where
  toText = Text.pack . show
  fromText = read . Text.unpack

data Transaction = Transaction
    { wtxId         :: ID Transaction
    , wtxExternalId :: Text
    , wtxAmount     :: Int64
    , wtxTime       :: UTCTime
    , wtxDepth      :: Int64
    , wtxStatus     :: !TxStatus
    , wtxMetadata   :: !Text
    } deriving (Generic)

instance SqlType TxStatus
instance SqlRow Transaction

data TransactionEntry = TransactionEntry
    { txEntryId      :: ID TransactionEntry
    , txEntryIndex   :: Maybe Int
    , txEntryAddress :: Text
    , txEntryAmount  :: Int64
    , txEntryInput   :: Bool
    , txEntryTxId    :: ID Transaction
    } deriving (Generic,Show)

instance SqlRow TransactionEntry

--------------------------------------------------------------------------------
-- | General lookup table

data Lookup = Lookup
  { lookupProp :: Text
  , lookupValue :: Text
  } deriving (Generic, Show)

instance SqlRow Lookup

--------------------------------------------------------------------------------
-- | Create Tables

lookupValues :: Table Lookup
lookupValues = table "lookup"
  [ #lookupProp :- primary ]

transactions :: Table Transaction
transactions = table "transaction"
  [ #wtxId :- autoPrimary
  , #wtxExternalId :- unique
  , #wtxTime :- index
  ]

transactionEntries :: Table TransactionEntry
transactionEntries = table "transaction_entry"
  [ #txEntryId :- autoPrimary
  , #txEntryTxId :- foreignKey transactions #wtxId
  ]

profileWallets :: Table ProfileWallet
profileWallets = table "profile_wallet"
  [ #profileWalletId :- primary
  , #profileWalletId :- foreignKey profiles #profileId
  ]

dapps :: Table DApp
dapps = table "dapp"
  [ #dappId :- unique
  , #dappId :- foreignKey profiles #profileId
  ]

