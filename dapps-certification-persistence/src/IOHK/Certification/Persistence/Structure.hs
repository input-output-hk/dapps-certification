{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
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
--import           Database.Selda.SqlType as Selda
import           GHC.OverloadedLabels
import           Data.Int

import           IOHK.Certification.Persistence.Structure.Profile
import           IOHK.Certification.Persistence.Structure.Subscription

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
-- | Certification
{-
data CertificationLevel = L0 | L1 | L2 | L3
  deriving (Generic,Show,Read,Eq,Ord,Enum)

instance SqlType CertificationLevel where
  mkLit n = LCustom TInt64 (LInt64 (toInt64 n))
    where
    toInt64 = \case
      L0 -> 0
      L1 -> 1
      L2 -> 2
      L3 -> 3
  sqlType _ = TInt64
  fromSql (SqlInt64 0) = L0
  fromSql (SqlInt64 1) = L1
  fromSql (SqlInt64 2) = L2
  fromSql (SqlInt64 3) = L3
  fromSql v            = throw $ userError $ "fromSql: expected SqlInt64, got " ++ show v
  defaultValue = mkLit L1

instance ToJSON CertificationLevel where
  toJSON = toJSON . \case
    L0 -> "l0" :: String
    L1 -> "l1"
    L2 -> "l2"
    L3 -> "l3"

instance FromJSON CertificationLevel where
  parseJSON = withText "CertificationLevel" $ \case
    "l0" -> pure L0
    "l1" -> pure L1
    "l2" -> pure L2
    "l3" -> pure L3
    _    -> fail "CertificationLevel"

instance ToSchema CertificationLevel where
   declareNamedSchema _ = do
     let values = [ "l0", "l1", "l2", "l3" ] :: [Value]
     return $ NamedSchema (Just "CertificationLevel") $ mempty
       & type_ ?~ SwaggerString
       & enum_ ?~ values
-}

data Certification = Certification
  { certId              :: ID Certification
  , certTransactionId   :: Text
  , certCreatedAt       :: UTCTime
  } deriving (Generic,Show)

instance FromJSON Certification where
    parseJSON = withObject "Certification" $ \v -> Certification def
      <$> v .: "transactionId"
      <*> v .: "createdAt"

instance ToJSON Certification where
  toJSON (Certification{..}) = object
      [ "transactionId" .= certTransactionId
      , "createdAt" .= certCreatedAt
      ]

instance ToSchema Certification where
   declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    utcSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
    return $ NamedSchema (Just "Certification") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("transactionId", textSchema)
          , ("createdAt", utcSchema)
          ]
      & required .~ [ "createdAt" ]

instance SqlRow Certification

-- one to one mapping with Run
data L1Certification = L1Certification
  { l1CertRunId :: UUID
  , l1CertId  :: ID Certification
  } deriving (Generic,Show)

instance SqlRow L1Certification

data L1CertificationDTO = L1CertificationDTO
  { l1Certification :: L1Certification
  , certification :: Certification
  }

instance ToJSON L1CertificationDTO where
  toJSON L1CertificationDTO{..} = Object (x <> y)
    where
    x = case toJSON certification of
            Object obj -> obj
            _          -> KM.empty
    y = KM.fromList [ "runId" .= l1CertRunId l1Certification ]

instance FromJSON L1CertificationDTO where
  parseJSON = withObject "L1CertificationDTO" $ \v -> do
    l1Certification <- L1Certification
      <$> v .: "runId"
      <*> pure def
    L1CertificationDTO l1Certification <$> v .: "runId"

instance ToSchema L1CertificationDTO where
  declareNamedSchema _ = do
    certificationSchema <- declareSchema (Proxy :: Proxy Certification)
    uuidSchema <- declareSchemaRef (Proxy :: Proxy UUID)
    return $ NamedSchema (Just "TierDTO") $ certificationSchema
              & properties %~ (`mappend` [ ("certRunId", uuidSchema) ])
              & required %~  (<> [ "certRunId" ])

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
-- | Run

data Status = Queued | Failed | Succeeded | ReadyForCertification | Certified | Aborted
  deriving (Show, Read, Bounded, Enum, Eq, Generic)

instance ToJSON Status where
  toJSON :: Status -> Value
  toJSON Queued    = toJSON ("queued" :: Text)
  toJSON Failed    = toJSON ("failed" :: Text)
  toJSON Succeeded = toJSON ("succeeded" :: Text)
  toJSON ReadyForCertification = toJSON ("ready-for-certification" :: Text)
  toJSON Certified = toJSON ("certified" :: Text)
  toJSON Aborted = toJSON ("aborted" :: Text)

instance FromJSON Status where
    parseJSON =
      withText "Status" handle
      where
        handle "queued" = pure Queued
        handle "failed" = pure Failed
        handle "succeeded" = pure Succeeded
        handle "certified" = pure Succeeded
        handle "ready-for-certification" = pure ReadyForCertification
        handle "aborted" = pure Aborted
        handle t = fail $ "provided text (" ++ show t ++ ") is not a Status"

instance SqlType Status

type CommitHash = Text
type CertificationPrice = Int64
data Run = Run
  { runId               :: UUID
  , created             :: UTCTime
  , finishedAt          :: Maybe UTCTime
  , syncedAt            :: UTCTime
  , repoUrl             :: Text
  , commitDate          :: UTCTime
  , commitHash          :: CommitHash
  , runStatus           :: Status
  , profileId           :: ID Profile
  , certificationPrice  :: CertificationPrice
  , reportContentId :: Maybe Text
  } deriving (Generic,Show)

instance ToSchema Status where
   declareNamedSchema _ = do
    let values = ["queued", "failed", "succeeded", "certified", "ready-for-certification","aborted"] :: [Value]
    return $ NamedSchema (Just "RunStatus") $ mempty
      & type_ ?~ SwaggerString
      & enum_ ?~ values

instance ToSchema Run where
   declareNamedSchema _ = do
    utcSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
    utcSchemaM <- declareSchemaRef (Proxy :: Proxy (Maybe UTCTime))
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    statusSchema <- declareSchemaRef (Proxy :: Proxy Status)
    uuidSchema <- declareSchemaRef (Proxy :: Proxy UUID)
    intSchema <- declareSchemaRef (Proxy :: Proxy Int)
    return $ NamedSchema (Just "Run") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("created", utcSchema)
          , ("runId", uuidSchema)
          , ("finishedAt", utcSchemaM)
          , ("syncedAt", utcSchema)
          , ("repoUrl", textSchema)
          , ("commitDate", utcSchema)
          , ("commitHash", textSchema)
          , ("runStatus", statusSchema)
          , ("certificationPrice", intSchema)
          , ("reportContentId", textSchema)
          ]
      & required .~ [ "runId", "created", "utcSchema", "repoUrl"
                    , "commitDate","commitHash", "runStatus", "certificationPrice"]

instance ToJSON Run where
  toJSON (Run{..}) = object
      [ "runId" .= runId
      , "created" .= created
      , "finishedAt" .= finishedAt
      , "syncedAt" .= syncedAt
      , "repoUrl" .= repoUrl
      , "commitDate" .= commitDate
      , "commitHash" .= commitHash
      , "runStatus" .= runStatus
      , "certificationPrice" .= certificationPrice
      , "reportContentId" .= reportContentId
      ]

instance FromJSON Run where
    parseJSON = withObject "Run" $ \v -> Run
        <$> v .: "runId"
        <*> v .: "created"
        <*> v .:? "finishedAt" .!= Nothing
        <*> v .: "syncedAt"
        <*> v .: "repoUrl"
        <*> v .: "commitDate"
        <*> v .: "commitHash"
        <*> v .: "runStatus"
        <*> pure def
        <*> v .: "certificationPrice"
        <*> v .:? "reportContentId" .!= Nothing

instance SqlRow Run
instance IsLabel "profileId" (ID Profile -> Profile -> Profile) where
  fromLabel v p = p { profileId = v}

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
runs :: Table Run
runs = table "run"
  [ #runId :- primary
  , #profileId :- foreignKey profiles #profileId
  , #created :- index
  ]

certifications :: Table Certification
certifications = table "certification"
  [ #certId :- primary
  ]

l1Certifications :: Table L1Certification
l1Certifications = table "certification"
  [ #l1CertRunId :- primary
  , #l1CertRunId :- foreignKey runs #runId
  , #l1CertId :- foreignKey certifications #certId
  ]

dapps :: Table DApp
dapps = table "dapp"
  [ #dappId :- unique
  , #dappId :- foreignKey profiles #profileId
  ]

createTables :: MonadSelda m => m ()
createTables = do
  createTable certifications
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
