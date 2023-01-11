{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}     -- allows to write Map and HashMap as lists
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module IOHK.Certification.Persistence.Structure where

import Database.Selda
import GHC.OverloadedLabels
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Swagger hiding (Contact)
import Control.Lens hiding ((.=),index)

data Profile = Profile
  { profileId :: ID Profile   -- TODO: do we need an internal id?
  , ownerAddress :: Text      -- TODO: type level restrictions might apply in the future
                              -- regarding the format
  , website :: Maybe Text
  , vendor :: Maybe Text
  , twitter :: Maybe Text
  , linkedin :: Maybe Text
  , authors :: Maybe Text
  , contacts :: Maybe Text
  } deriving (Generic, Show)

type ProfileId = ID Profile

instance FromJSON Profile where
    parseJSON = withObject "Profile" $ \v -> Profile
      <$> (pure def)
      <*> v .: "address"
      <*> v .:? "website"  .!= Nothing
      <*> v .:? "vendor"  .!= Nothing
      <*> v .:? "twitter"  .!= Nothing
      <*> v .:? "linkedin"  .!= Nothing
      <*> v .:? "authors"  .!= Nothing
      <*> v .:? "contacts"  .!= Nothing

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

data Certification = Certification
  { certRunId :: UUID
  , certReportContentId :: Text
  , certTransactionId :: Text
  , certCreatedAt :: UTCTime
  } deriving (Generic,Show)

instance FromJSON Certification where
    parseJSON = withObject "Certification" $ \v -> Certification
      <$> v .: "runId"
      <*> v .: "reportContentId"
      <*> v .: "transactionId"
      <*> v .: "createdAt"

instance ToJSON Certification where
  toJSON (Certification{..}) = object
      [ "reportContentId" .= certReportContentId
      , "transactionId" .= certTransactionId
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
          [ ("reportContentId", textSchema)
          , ("transactionId", textSchema)
          , ("createdAt", utcSchema)
          , ("runId", uuidSchema)
          ]
      & required .~ [ "runId", "createdAt", "reportContentId" ]

instance SqlRow Certification

data DApp = DApp
  { dappId :: ID Profile
  , dappName :: Text
  , dappOwner :: Text
  , dappRepo :: Text
  , dappVersion :: Text
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
          ]
      & required .~ ["name", "owner", "repo", "version"]

instance FromJSON DApp where
  parseJSON = withObject "DApp" $ \v -> DApp
    <$> pure def
    <*> v .: "name"
    <*> v .: "owner"
    <*> v .: "repo"
    <*> v .: "version"

instance ToJSON DApp where
  toJSON (DApp{..}) = object
      [ "name" .= dappName
      , "owner" .= dappOwner
      , "repo" .= dappRepo
      , "version" .= dappVersion
      ]

instance SqlRow DApp

data Status = Queued | Failed | Succeeded | Certified
  deriving (Show, Read, Bounded, Enum, Eq, Generic)

instance ToJSON Status where
  toJSON :: Status -> Value
  toJSON Queued = toJSON ("queued" :: Text)
  toJSON Failed = toJSON ("failed" :: Text)
  toJSON Succeeded = toJSON ("succeeded" :: Text)
  toJSON Certified = toJSON ("certified" :: Text)

instance FromJSON Status where
    parseJSON =
      withText "Status" handle
      where
        handle "queued" = pure Queued
        handle "failed" = pure Failed
        handle "succeeded" = pure Succeeded
        handle "certified" = pure Succeeded
        handle t = fail $ "provided text (" ++ show t ++ ") is not a Status"

instance SqlType Status

type CommitHash = Text
data Run = Run
  { runId :: UUID
  , created :: UTCTime
  , finishedAt :: Maybe UTCTime
  , syncedAt :: UTCTime
  , repoUrl :: Text
  , commitDate :: UTCTime
  , commitHash :: CommitHash
  , runStatus :: Status
  , profileId :: ID Profile
  } deriving (Generic,Show)

instance ToSchema Status
instance ToSchema Run where
   declareNamedSchema _ = do
    utcSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
    utcSchemaM <- declareSchemaRef (Proxy :: Proxy (Maybe UTCTime))
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    statusSchema <- declareSchemaRef (Proxy :: Proxy Status)
    return $ NamedSchema (Just "Run") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("created", utcSchema)
          , ("finishedAt", utcSchemaM)
          , ("syncedAt", utcSchema)
          , ("repoUrl", textSchema)
          , ("commitDate", utcSchema)
          , ("commitHash", textSchema)
          , ("runStatus", statusSchema)
          , ("certificateCreatedAt", utcSchemaM)
          ]
      & required .~ [ "created", "utcSchema", "repoUrl", "commitDate","commitHash", "runStatus" ]

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
        <*> (pure def)

instance SqlRow Run

instance IsLabel "profileId" (ID Profile -> Profile -> Profile) where
  fromLabel = \v p -> p { profileId = v}

data ProfileDTO = ProfileDTO
  { profile :: Profile
  , dapp :: Maybe DApp
  }

instance FromJSON ProfileDTO where
  parseJSON = withObject "ProfileDTO" $ \v -> ProfileDTO
      <$> parseJSON (Object v)
      <*> v .:? "dapp" .!= Nothing

instance ToJSON ProfileDTO where
  toJSON ProfileDTO{..} = object $
      [ "dapp" .= dapp ] ++ profileJSONPairs profile

instance ToSchema ProfileDTO where
  declareNamedSchema _ = do
    profileSchema <- declareSchema (Proxy :: Proxy Profile)
    dappSchema <- declareSchemaRef (Proxy :: Proxy DApp)
    return $ NamedSchema (Just "ProfileDTO") $ profileSchema
              & properties %~ (`mappend` [ ("dapp", dappSchema) ])

profiles :: Table Profile
profiles = table "profile"
  [ #profileId :- autoPrimary
  , #ownerAddress :- unique
  , #ownerAddress :- index
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
  createTable dapps
  createTable runs
