{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module IOHK.Certification.Persistence.Structure.Profile where

import           Control.Lens         hiding (index, (.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Swagger         hiding (Contact)
import           Database.Selda
import           Data.Int
import           IOHK.Certification.Persistence.Pattern

--------------------------------------------------------------------------------
-- | Profile

data Profile = Profile
  { profileId     :: ID Profile
  , ownerAddress  :: ProfileWalletAddress
  , website       :: Maybe Website
  , twitter       :: Maybe Twitter
  , linkedin      :: Maybe LinkedIn
  , email         :: Maybe Email
  , contactEmail  :: Maybe Email
  , companyName   :: Maybe Text
  , fullName      :: Maybe Text
  } deriving (Generic, Show, Eq)

type ProfileId = ID Profile
instance ToSchema ProfileId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

--------------------------------------------------------------------------------
-- | FIELD TYPES

instance ToSchema Profile where
   declareNamedSchema _ = do
    websiteSchema <- declareSchemaRef (Proxy :: Proxy Website)
    addressSchema <- declareSchemaRef (Proxy :: Proxy ProfileWalletAddress)
    linkedInSchema <- declareSchemaRef (Proxy :: Proxy LinkedIn)
    emailSchema <- declareSchemaRef (Proxy :: Proxy Email)
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    twitterSchema <- declareSchemaRef (Proxy :: Proxy Twitter)
    return $ NamedSchema (Just "Profile") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("address", addressSchema)
          , ("website", websiteSchema)
          , ("twitter", twitterSchema)
          , ("linkedin", linkedInSchema)
          , ("email", emailSchema)
          , ("contactEmail", emailSchema)
          , ("companyName", textSchema)
          , ("fullName", textSchema)
          ]
      & required .~ [ "address" ]

instance FromJSON Profile where
    parseJSON = withObject "Profile" $ \v ->
      Profile . toId
        <$> v .:? "profileId" .!= (-1)
        <*> v .:  "address"
        <*> v .:? "website"
        <*> v .:? "twitter"
        <*> v .:? "linkedin"
        <*> v .:? "email"
        <*> v .:? "contactEmail"
        <*> v .:? "companyName"
        <*> v .:? "fullName"

instance ToJSON Profile where
  toJSON = object . profileJSONPairs

profileJSONPairs :: Profile -> [Pair]
profileJSONPairs Profile{email = email',..} =
  [ "address"      .= ownerAddress
  , "website"      .= website
  , "twitter"      .= twitter
  , "linkedin"     .= linkedin
  , "email"        .= email'
  , "contactEmail" .= contactEmail
  , "companyName"  .= companyName
  , "fullName"     .= fullName
  ]
instance SqlRow Profile

profiles :: Table Profile
profiles = table "profile"
  [ #profileId :- autoPrimary
  , #ownerAddress :- unique
  , #ownerAddress :- index
  ]
