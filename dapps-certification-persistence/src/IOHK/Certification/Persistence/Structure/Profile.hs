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

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module IOHK.Certification.Persistence.Structure.Profile where

import           Control.Lens         hiding (index, (.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Swagger         hiding (Contact)
import           Database.Selda
import           Data.Int

--------------------------------------------------------------------------------
-- | Profile 

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

profiles :: Table Profile
profiles = table "profile"
  [ #profileId :- autoPrimary
  , #ownerAddress :- unique
  , #ownerAddress :- index
  ]

