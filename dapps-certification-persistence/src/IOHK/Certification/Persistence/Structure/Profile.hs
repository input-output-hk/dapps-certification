{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}

module IOHK.Certification.Persistence.Structure.Profile where

import           Control.Lens         hiding (index, (.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Swagger         hiding (Contact)
import           Database.Selda
import           Database.Selda.SqlType
import           Data.Int
import           IOHK.Certification.Persistence.Pattern
import           Control.Exception ( throw)
import           Data.Text hiding (index)

data UserRole = NoRole | Support | Admin
  deriving (Generic,Show,Read,Eq,Enum)

instance ToJSON UserRole where
  toJSON NoRole  = toJSON ("no-role" :: String)
  toJSON Support = toJSON ("support" :: String)
  toJSON Admin   = toJSON ("admin" :: String)

instance FromJSON UserRole where
  parseJSON = withText "UserRole" $ \case
    "no-role" -> pure NoRole
    "support" -> pure Support
    "admin" -> pure Admin
    _ -> fail "Invalid user role"

instance ToSchema UserRole where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "UserRole") $ mempty
      & type_ ?~ SwaggerString
      & enum_ ?~ [ "no-role", "support", "admin" ]

instance ToParamSchema UserRole where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ [ "no-role", "support", "admin" ]

instance Num UserRole where
  fromInteger = userRoleFromNum
  (+) a b = userRoleToNum a + userRoleToNum b
  (-) a b = userRoleToNum a - userRoleToNum b
  (*) a b = userRoleToNum a * userRoleToNum b
  abs a = abs $ userRoleToNum a
  signum a = signum $ userRoleToNum a


userRoleToNum :: Num a => UserRole -> a
userRoleToNum NoRole = 0
userRoleToNum Support = 1
-- we leave a gap here for future roles
userRoleToNum Admin   = 100

userRoleFromNum :: (Eq a, Num a,Ord a) => a -> UserRole
userRoleFromNum x
  | x <= 0 = NoRole
  | x < 100 = Support
  | otherwise = Admin

instance Ord UserRole where
  compare a b = compare @Int (userRoleToNum a) (userRoleToNum b)

instance SqlType UserRole where
  mkLit n = LCustom TInt64 (LInt32 (userRoleToNum n))

  sqlType _ = TInt32
  fromSql (SqlInt32 x) = userRoleFromNum x
  fromSql (SqlInt64 x) = userRoleFromNum x
  fromSql v            = throw $ userError $ "fromSql: expected SqlInt64, got " ++ show v
  defaultValue = mkLit NoRole

data ProfileRole = ProfileRole
  { profileId :: ID Profile
  , role :: UserRole
  } deriving (Generic,Show,Eq)

instance SqlRow ProfileRole

instance ToParamSchema ProfileId where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerInteger

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

instance FromJSON (ID Profile) where
  parseJSON = withText "ID Profile" $ \t -> pure $ toId $ read $ unpack t

instance ToJSON (ID Profile) where
  toJSON = toJSON . show . fromId

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

profileRoles :: Table ProfileRole
profileRoles = table "profile_role"
  [ #profileId :- foreignKey profiles #profileId ]

