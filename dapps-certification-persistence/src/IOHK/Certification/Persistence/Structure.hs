{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module IOHK.Certification.Persistence.Structure where

import Database.Selda
import GHC.OverloadedLabels
import Data.Aeson

data Profile = Profile
  { profileId :: ID Profile -- TODO: do we need an internal id?
  , ownerAddress :: Text    -- TODO: type level restrictions might apply in the future
                            -- regarding the format
  , dapp :: Maybe Text
  , website :: Maybe Text
  , vendor :: Maybe Text
  , twitter :: Maybe Text
  , linkedin :: Maybe Text
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

instance ToJSON Profile where
  toJSON (Profile{..}) = object
      [ "address" .= ownerAddress
      , "dapp" .= dapp
      , "website" .= website
      , "vendor" .= vendor
      , "twitter" .= twitter
      , "linkedin" .= linkedin
      ]
instance SqlRow Profile

data Author = Author
  { authorId :: ID Author
  , name :: Text
  , profileId :: ID Profile
  } deriving (Generic,Show)

instance SqlRow Author

instance IsLabel "profileId" (ID Profile -> Author -> Author) where
  fromLabel = \v p -> p { profileId = v}

data Contact = Contact
  { contactId :: ID Contact
  , name :: Text
  , details :: Maybe Text
  , profileId :: ID Profile
  } deriving (Generic,Show)

instance IsLabel "profileId" (ID Profile -> Contact -> Contact) where
  fromLabel = \v p -> p { profileId = v}

instance SqlRow Contact

data Status = Queued | Failed | Succeeded
  deriving (Show, Read, Bounded, Enum, Eq)

instance ToJSON Status where
  toJSON :: Status -> Value
  toJSON Queued = toJSON ("queued" :: Text)
  toJSON Failed = toJSON ("failed" :: Text)
  toJSON Succeeded = toJSON ("succeeded" :: Text)

instance FromJSON Status where
    parseJSON =
      withText "Status" handle
      where
        handle "queued" = pure Queued
        handle "failed" = pure Failed
        handle "succeeded" = pure Succeeded
        handle t = fail $ "provided text (" ++ show t ++ ") is not a Status"

instance SqlType Status

data Run = Run
  { runId :: UUID
  , created :: UTCTime
  , finishedAt :: Maybe UTCTime
  , syncedAt :: UTCTime
  , repoUrl :: Text
  , commitDate :: UTCTime
  , runStatus :: Status
  , profileId :: ID Profile
  , certificateCreatedAt :: Maybe UTCTime
  } deriving (Generic,Show)

instance ToJSON Run where
  toJSON (Run{..}) = object
      [ "runId" .= runId
      , "created" .= created
      , "finishedAt" .= finishedAt
      , "syncedAt" .= syncedAt
      , "repoUrl" .= repoUrl
      , "commitDate" .= commitDate
      , "runStatus" .= runStatus
      , "certificateCreatedAt" .= certificateCreatedAt
      ]

instance FromJSON Run where
    parseJSON = withObject "Run" $ \v -> Run
        <$> v .: "runId"
        <*> v .: "created"
        <*> v .:? "finishedAt" .!= Nothing
        <*> v .: "syncedAt"
        <*> v .: "repoUrl"
        <*> v .: "commitDate"
        <*> v .: "runStatus"
        <*> (pure def)
        <*> v .:? "certificateCreatedAt" .!= Nothing

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

authors :: Table Author
authors = table "author"
  [ #authorId :- primary
  , #profileId :- foreignKey profiles #profileId
  ]

contacts :: Table Contact
contacts = table "contact"
  [ #contactId :- primary
  , #profileId :- foreignKey profiles #profileId
  ]


createTables :: MonadSelda m => m ()
createTables = do
  createTable profiles
  createTable authors
  createTable contacts
  createTable runs



