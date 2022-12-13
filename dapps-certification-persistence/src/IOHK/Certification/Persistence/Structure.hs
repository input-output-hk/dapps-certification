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
import Data.Proxy
import Data.Swagger hiding (Contact)
import Control.Lens hiding ((.=),index)

data Profile = Profile
  { profileId :: ID Profile   -- TODO: do we need an internal id?
  , ownerAddress :: Text      -- TODO: type level restrictions might apply in the future
                              -- regarding the format
  , dapp :: Maybe Text
  , website :: Maybe Text
  , vendor :: Maybe Text
  , twitter :: Maybe Text
  , linkedin :: Maybe Text
  , authors :: Maybe Text
  , contacts :: Maybe Text
  , version :: Maybe Text
  } deriving (Generic, Show)

type ProfileId = ID Profile

instance FromJSON Profile where
    parseJSON = withObject "Profile" $ \v -> Profile
      <$> (pure def)
      <*> v .: "address"
      <*> v .:? "dapp"  .!= Nothing
      <*> v .:? "website"  .!= Nothing
      <*> v .:? "vendor"  .!= Nothing
      <*> v .:? "twitter"  .!= Nothing
      <*> v .:? "linkedin"  .!= Nothing
      <*> v .:? "authors"  .!= Nothing
      <*> v .:? "contacts"  .!= Nothing
      <*> v .:? "version"  .!= Nothing

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
          , ("version", textSchemaM)
          ]
      & required .~ [ "address", "dapp" ]

instance ToJSON Profile where
  toJSON (Profile{..}) = object
      [ "address" .= ownerAddress
      , "dapp" .= dapp
      , "website" .= website
      , "vendor" .= vendor
      , "twitter" .= twitter
      , "linkedin" .= linkedin
      , "authors" .= authors
      , "contacts" .= contacts
      , "version" .= version
      ]
instance SqlRow Profile

data Certification = Certification
  { certId :: ID Certification
  , certReportContentId :: Text
  , certCreatedAt :: UTCTime
  , certRunId :: UUID
  } deriving (Generic,Show)

instance FromJSON Certification where
    parseJSON = withObject "Certification" $ \v -> Certification
      <$> (pure def)
      <*> v .: "reportContentId"
      <*> v .: "createdAt"
      <*> v .: "runId"

instance ToJSON Certification where
  toJSON (Certification{..}) = object
      [ "reportContentId" .= certReportContentId
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
          , ("createdAt", utcSchema)
          , ("runId", uuidSchema)
          ]
      & required .~ [ "runId", "createdAt", "reportContentId" ]

instance SqlRow Certification

data DApp = DApp
  { dappId :: ID DApp
  , dappName :: Text
  , dappOwner :: Text
  , dappRepo :: Text
  , dappProfileId :: ID Profile
  } deriving (Generic,Show)

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
  [ #certId :- primary
  , #certRunId :- foreignKey runs #runId
  , #certRunId :- unique
  ]

dapps :: Table DApp
dapps = table "dapp"
  [ #dappId :- primary
  , #dappProfileId :- foreignKey profiles #profileId
  , #dappProfileId :- unique
  ]

createTables :: MonadSelda m => m ()
createTables = do
  createTable certifications
  createTable profiles
  createTable dapps
  createTable runs
