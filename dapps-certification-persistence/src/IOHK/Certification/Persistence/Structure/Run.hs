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
{-# LANGUAGE LambdaCase                 #-}

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IOHK.Certification.Persistence.Structure.Run where



import           Control.Lens         hiding (index, (.=))
import           Data.Aeson
import           Data.Proxy
--import           Control.Exception ( throw)
import           Data.Swagger         hiding (Contact)
import           Database.Selda
import           Database.Selda.SqlType as Selda
import           GHC.OverloadedLabels
import           Data.Int

import           IOHK.Certification.Persistence.Structure.Profile
import           Control.Exception ( throw)

import qualified Data.Text         as Text
import qualified Data.Aeson.KeyMap as KM

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


runs :: Table Run
runs = table "run"
  [ #runId :- primary
  , #profileId :- foreignKey profiles #profileId
  , #created :- index
  ]

