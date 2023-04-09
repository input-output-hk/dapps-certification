{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module IOHK.Certification.Persistence.Structure where

import           Control.Lens         hiding (index, (.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Swagger         hiding (Contact)
import           Database.Selda
import           Database.Selda.SqlType as Selda
import           Control.Exception ( throw)
import           GHC.OverloadedLabels
import           Data.Char as Char
import           Data.Int (Int64)

import qualified Data.Text            as Text

newtype IpfsCid = IpfsCid { ipfsCid :: Text}
  deriving (ToJSON,FromJSON,Show )

newtype TxId = TxId { txId :: Text}
  deriving (ToJSON,FromJSON,Show,Read)

data Profile = Profile
  { profileId    :: ID Profile   -- TODO: do we need an internal id?
  , ownerAddress :: Text         -- TODO: type level restrictions might apply in the future
                                 -- regarding the format
  , website      :: Maybe Text
  , vendor       :: Maybe Text
  , twitter      :: Maybe Text
  , linkedin     :: Maybe Text
  , authors      :: Maybe Text
  , contacts     :: Maybe Text
  } deriving (Generic, Show)

type ProfileId = ID Profile

instance ToJSON ProfileId where
   toJSON = toJSON . show . fromId

instance ToJSON (ID ProfileId) where
   toJSON = toJSON . show . fromId

instance FromJSON ProfileId where
   parseJSON = withText "ID Profile" $ \t -> pure $ toId $ read $ Text.unpack t

instance ToSchema ProfileId where
   declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance FromJSON Profile where
    parseJSON = withObject "Profile" $ \v -> Profile def
      <$> v .:  "address"
      <*> v .:? "website"     .!= Nothing
      <*> v .:? "vendor"      .!= Nothing
      <*> v .:? "twitter"     .!= Nothing
      <*> v .:? "linkedin"    .!= Nothing
      <*> v .:? "authors"     .!= Nothing
      <*> v .:? "contacts"    .!= Nothing

instance ToSchema Profile where
   declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    textSchemaM <- declareSchemaRef (Proxy :: Proxy (Maybe Text))
    return $ NamedSchema (Just "Profile") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("address", textSchema)
          , ("dapp", textSchemaM)
          , ("website", textSchemaM)
          , ("vendor", textSchemaM)
          , ("twitter", textSchemaM)
          , ("linkedin", textSchemaM)
          , ("authors", textSchemaM)
          , ("contacts", textSchemaM)
          , ("githubToken", textSchemaM)
          ]
      & required .~ [ "address", "dapp" ]

instance ToJSON Profile where
  toJSON = object . profileJSONPairs

profileJSONPairs :: Profile -> [Pair]
profileJSONPairs Profile{..} =
  [ "address" .= ownerAddress
  , "website" .= website
  , "vendor" .= vendor
  , "twitter" .= twitter
  , "linkedin" .= linkedin
  , "authors" .= authors
  , "contacts" .= contacts
  ]
instance SqlRow Profile

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
   fromSql v            = throw $ userError $ "fromSql: expected SqlInt64, got " ++ show v
   defaultValue = mkLit Overlapping

data ProfileWallet = ProfileWallet
  { profileWalletId :: ID Profile
  , profileWalletAddress :: Text
  , profileWalletStatus :: WalletAddressStatus
  , profileWalletCredits :: Int64
  } deriving (Generic, Show, Eq)

instance SqlRow ProfileWallet

dropAndLowerFirst :: Int -> String ->  String
dropAndLowerFirst n = toLowerFirst . drop n
  where
  toLowerFirst :: String -> String
  toLowerFirst [] = []
  toLowerFirst (x:xs) = Char.toLower x : xs

instance ToJSON ProfileWallet where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 14 }

instance FromJSON ProfileWallet where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 14 }

instance ToSchema ProfileWallet where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions { fieldLabelModifier = dropAndLowerFirst 14 }

data Certification = Certification
  { certRunId           :: UUID
  , certTransactionId   :: Text
  , certCreatedAt       :: UTCTime
  } deriving (Generic,Show)

instance FromJSON Certification where
    parseJSON = withObject "Certification" $ \v -> Certification
      <$> v .: "runId"
      <*> v .: "transactionId"
      <*> v .: "createdAt"

instance ToJSON Certification where
  toJSON (Certification{..}) = object
      [ "transactionId" .= certTransactionId
      , "createdAt" .= certCreatedAt
      , "runId" .= certRunId
      ]

instance ToSchema Certification where
   declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    utcSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
    uuidSchema <- declareSchemaRef (Proxy :: Proxy UUID)
    return $ NamedSchema (Just "Certification") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("transactionId", textSchema)
          , ("createdAt", utcSchema)
          , ("runId", uuidSchema)
          ]
      & required .~ [ "runId", "createdAt" ]

instance SqlRow Certification

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
type CertificationPrice = Int
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
    , wtxDepth      :: Int
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
transactionEntries = table "transaction_entry"
  [ #txEntryId :- autoPrimary
  , #txEntryTxId :- foreignKey transactions #wtxId
  ]
profiles :: Table Profile
profiles = table "profile"
  [ #profileId :- autoPrimary
  , #ownerAddress :- unique
  , #ownerAddress :- index
  ]

profileWallets :: Table ProfileWallet
profileWallets = table "profile_wallet"
  [ #profileWalletId :- primary
  , #profileWalletId :- foreignKey profiles #profileId
  ]

runs :: Table Run
runs = table "run"
  [ #runId :- primary
  , #profileId :- foreignKey profiles #profileId
  , #created :- index
  ]

certifications :: Table Certification
certifications = table "certification"
  [ #certRunId :- primary
  , #certRunId :- foreignKey runs #runId
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
  createTable profileWallets
  createTable dapps
  createTable runs
  createTable transactions
  createTable transactionEntries
