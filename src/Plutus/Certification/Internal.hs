{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Plutus.Certification.Internal where
import           Data.Aeson
import           GHC.Generics
import           Data.Proxy
import           GHC.TypeLits
import           Data.Text as Text
import           Control.Monad.Catch (MonadMask)

import qualified IOHK.Certification.Persistence as DB
import Control.Monad.RWS

newtype JSONCustomOptions n a = JSONCustomOptions a deriving Generic

defaultRecordTypeOptions :: Int -> Options
defaultRecordTypeOptions n = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . Prelude.drop n
  , constructorTagModifier = camelTo2 '_' . Prelude.drop n
  }

instance (Generic a, GToJSON' Value Zero (Rep a),KnownNat n) => ToJSON (JSONCustomOptions n a)
    where
    toJSON (JSONCustomOptions x) = genericToJSON (defaultRecordTypeOptions (nToDrop @n)) x

nToDrop :: forall n. KnownNat n => Int
nToDrop = fromInteger $ natVal (Proxy :: Proxy n)

instance (Generic a, GFromJSON Zero (Rep a), KnownNat n) => FromJSON (JSONCustomOptions n a)
  where
  parseJSON = fmap JSONCustomOptions . genericParseJSON (defaultRecordTypeOptions (nToDrop @n))

splitString :: Int -> Text -> Value
splitString maxChars = toValue . chunksOf maxChars
    where
    toValue []  = toJSON ("" :: Text)
    toValue [x] = toJSON x
    toValue xs  = toJSON xs

split64 :: Text -> Value
split64 = splitString 64

type WithDB = forall a m. (MonadIO m, MonadMask m) => (forall n. (DB.MonadSelda n,MonadMask n) => n a) -> m a

newtype WithDBWrapper = WithDBWrapper WithDB

class HasDb env where
  getWithDb :: env -> WithDB

instance HasDb WithDBWrapper where
  getWithDb (WithDBWrapper db') = db'

withDb :: (MonadReader env m,HasDb env, MonadIO m,MonadMask m) => (forall n. (DB.MonadSelda n,MonadMask n) => n a) -> m a
withDb dbAction = do
  env <- ask
  getWithDb env dbAction
