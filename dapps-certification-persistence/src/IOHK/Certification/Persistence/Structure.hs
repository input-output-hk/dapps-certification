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

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IOHK.Certification.Persistence.Structure where

import           Control.Lens         hiding (index, (.=))
import           Data.Aeson
import           Data.Proxy
--import           Control.Exception ( throw)
import           Data.Swagger         hiding (Contact)
import           Database.Selda
import           Data.Int

import           IOHK.Certification.Persistence.Structure.Profile
import           IOHK.Certification.Persistence.Structure.Subscription
import           IOHK.Certification.Persistence.Structure.Certification
import           IOHK.Certification.Persistence.Structure.Run

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

--------------------------------------------------------------------------------
-- | Profile

instance FromJSON (ID Profile) where
  parseJSON = withText "ID Profile" $ \t -> pure $ toId $ read $ Text.unpack t

instance ToJSON (ID Profile) where
  toJSON = toJSON . show . fromId

data ProfileDTO = ProfileDTO
  { profile :: Profile
  , dapp    :: Maybe DApp
  }

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
  , dappVersion :: Text
  , dappGitHubToken :: Maybe Text
  } deriving (Generic,Show)

instance ToSchema DApp where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    return $ NamedSchema (Just "DApp") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("name", textSchema)
          , ("owner", textSchema)
          , ("repo", textSchema)
          , ("version", textSchema)
          , ("githubToken", textSchema)
          ]
      & required .~ ["name", "owner", "repo", "version"]

instance FromJSON DApp where
  parseJSON = withObject "DApp" $ \v -> DApp def
    <$> v .: "name"
    <*> v .: "owner"
    <*> v .: "repo"
    <*> v .: "version"
    <*> v .: "githubToken"

instance ToJSON DApp where
  toJSON (DApp{..}) = object
      [ "name" .= dappName
      , "owner" .= dappOwner
      , "repo" .= dappRepo
      , "version" .= dappVersion
      , "githubToken" .= fmap (const ("<<REDACTED>>" :: Text)) dappGitHubToken
      ]

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
-- | Create Tables

transactions :: Table Transaction
transactions = table "transaction"
  [ #wtxId :- autoPrimary
  , #wtxExternalId :- unique
  , #wtxTime :- index
  ]

transactionEntries :: Table TransactionEntry
transactionEntries = table "entry"
  [ #txEntryId :- autoPrimary
  , #txEntryTxId :- foreignKey transactions #wtxId
  ]
dapps :: Table DApp
dapps = table "dapp"
  [ #dappId :- unique
  , #dappId :- foreignKey profiles #profileId
  ]

createTables :: MonadSelda m => m ()
createTables = do
  createTable certifications
  createTable onChainCertifications
  createTable profiles
  createTable dapps
  createTable runs
  createTable transactions
  createTable transactionEntries
  createTable features
  createTable tiers
  createTable tierFeatures
  createTable subscriptions
  createTable l1Certifications
