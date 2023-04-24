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

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module IOHK.Certification.Persistence.Structure.Subscription where

import Data.Maybe
import Database.Selda
import Database.Selda.SqlType as Selda
import IOHK.Certification.Persistence.Structure.Profile
import Data.Text hiding (drop)
import Control.Exception ( throw)
import Data.Aeson
import Data.Swagger
import Data.Char as Char
import Control.Lens
import Data.Proxy
import Data.Int
import qualified Data.Text as Text

data FeatureType
  = L1Run
  | L2UploadReport
  | L0UploadReport
  deriving (Eq,Generic, Show, Bounded, Enum, Read)

instance SqlType FeatureType

instance ToJSON FeatureType where
  toJSON = \case
    L1Run -> "l1-run"
    L2UploadReport -> "l2-upload-report"
    L0UploadReport -> "l0-upload-report"

instance FromJSON FeatureType where
  parseJSON = withText "FeatureType" $ \case
    "l1-run" -> pure L1Run
    "l2-upload-report" -> pure L2UploadReport
    "l0-upload-report" -> pure L0UploadReport
    _ -> fail "FeatureType not recognized"

instance ToSchema FeatureType where
  declareNamedSchema _ = do
    let values = [ "l1-run", "l2-upload-report", "l0-upload-report" ] :: [Value]
    pure $ NamedSchema (Just "FeatureType") $ mempty
      & type_ ?~ SwaggerString
      & enum_ ?~ values

data TierType = Developer | Auditor
                 deriving (Eq,Generic, Show, Bounded, Enum, Read)
instance SqlType TierType

instance ToJSON TierType where
  toJSON = \case
    Developer -> "developer"
    Auditor -> "auditor"

instance FromJSON TierType where
  parseJSON = withText "TierType" $ \case
    "developer" -> pure Developer
    "auditor" -> pure Auditor
    _ -> fail "TierType not recognized"

instance ToSchema TierType where
  declareNamedSchema _ = do
    let values = [ "developer", "auditor" ] :: [Value]
    pure $ NamedSchema (Just "TierType") $ mempty
      & type_ ?~ SwaggerString
      & enum_ ?~ values

data Feature = Feature
  -- TODO: make sure featureType serializes properly for an id
  { featureId :: FeatureType
  , featureName :: Text
  } deriving (Generic, Show)

dropAndLowerFirst :: Int -> String ->  String
dropAndLowerFirst n = toLowerFirst . drop n
  where
  toLowerFirst :: String -> String
  toLowerFirst [] = []
  toLowerFirst (x:xs) = Char.toLower x : xs

instance ToJSON Feature where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 7 }

instance FromJSON Feature where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 7 }

instance ToSchema Feature where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions { fieldLabelModifier = dropAndLowerFirst 7 }

instance SqlRow Feature

features :: Table Feature
features = tableFieldMod "feature"
  [ #featureId :- primary
  , #featureName :- unique
  ] (fromJust . stripPrefix "feature")

data Tier = Tier
  { tierId :: ID Tier
  , tierName :: Text
  , tierSubtitle :: Text
  , tierDescription :: Text
  , tierType :: TierType
  , tierUsdPrice :: Double
  , tierDuration :: Int
  , tierEnabled :: Bool
  } deriving (Generic, Show)

type TierId = ID Tier

instance ToJSON Tier where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 4 }

instance FromJSON Tier where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropAndLowerFirst 4 }

instance ToSchema Tier where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions { fieldLabelModifier = dropAndLowerFirst 4 }

instance ToSchema TierId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance FromJSON (ID Tier) where
  parseJSON = withText "ID Tier" $ \t -> pure $ toId $ read $ Text.unpack t


instance ToJSON (ID Tier) where
  toJSON = toJSON . show . fromId
instance ToParamSchema TierId where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerInteger

instance SqlRow Tier

tiers :: Table Tier
tiers = tableFieldMod "tier"
  [ #tierId :- autoPrimary
  ] (fromJust . stripPrefix "tier")

data TierFeature = TierFeature
  { tierFeatureId :: ID TierFeature
  , tierFeatureTierId :: ID Tier
  , tierFeatureFeatureId :: FeatureType
  } deriving (Generic, Show)

instance SqlRow TierFeature

tierFeatures :: Table TierFeature
tierFeatures = tableFieldMod "tier_feature"
  [ #tierFeatureId :- autoPrimary
  , #tierFeatureTierId :- foreignKey tiers #tierId
  , #tierFeatureFeatureId :- foreignKey features #featureId
  ] (fromJust . stripPrefix "tierFeature")

data SubscriptionStatus = ActiveSubscription | InactiveSubscription | PendingSubscription
                       deriving (Generic, Show, Bounded, Enum, Read)
data Subscription = Subscription
  { subscriptionId :: ID Subscription
  , subscriptionProfileId :: ID Profile
  , subscriptionTierId :: ID Tier
  , subscriptionName :: Text
  , subscriptionType :: TierType
  , subscriptionPrice :: Int64
  , subscriptionAdaUsdPrice :: Double
  , subscriptionStartDate :: UTCTime
  , subscriptionEndDate :: UTCTime
  , subscriptionStatus :: SubscriptionStatus
  } deriving (Generic, Show)

type SubscriptionId = ID Subscription

instance ToJSON (ID Subscription) where
  toJSON = toJSON . show . fromId

instance FromJSON (ID Subscription) where
  parseJSON = withText "ID Subscription" $ \t -> pure $ toId $ read $ Text.unpack t

instance ToParamSchema SubscriptionId where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerInteger

instance ToSchema SubscriptionId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToJSON SubscriptionStatus where
  toJSON = \case
    InactiveSubscription -> "inactive"
    ActiveSubscription -> "active"
    PendingSubscription -> "pending"

instance FromJSON SubscriptionStatus where
  parseJSON = withText "SubscriptionStatus" $ \case
    "inactive" -> pure InactiveSubscription
    "active" -> pure ActiveSubscription
    "pending" -> pure PendingSubscription
    _ -> fail "SubscriptionStatus must be one of: inactive, active, pending"

instance ToSchema SubscriptionStatus where
   declareNamedSchema _ = do
    let values = [ "inactive", "active", "pending" ] :: [Value]
    return $ NamedSchema (Just "SubscriptionStatus") $ mempty
      & type_ ?~ SwaggerString
      & enum_ ?~ values

instance SqlType SubscriptionStatus where
  mkLit n = LCustom TInt64 (LInt64 (toInt64 n))
    where
    toInt64 = \case
      InactiveSubscription -> 0
      ActiveSubscription -> 1
      PendingSubscription -> 2
  sqlType _ = TInt64
  fromSql (SqlInt64 0) = InactiveSubscription
  fromSql (SqlInt64 1) = ActiveSubscription
  fromSql (SqlInt64 2) = PendingSubscription
  fromSql v            = throw $ userError $ "fromSql: expected SqlInt64, got " ++ show v
  defaultValue = mkLit PendingSubscription

instance SqlRow Subscription

subscriptions :: Table Subscription
subscriptions = tableFieldMod "subscription"
  [ #subscriptionId :- autoPrimary
  , #subscriptionProfileId :- foreignKey profiles #profileId
  , #subscriptionTierId :- foreignKey tiers #tierId
  ] (fromJust . stripPrefix "subscription")
