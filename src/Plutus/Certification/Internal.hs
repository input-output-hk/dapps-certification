{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
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

module Plutus.Certification.Internal where
import           Data.Aeson
import           GHC.Generics
import           Data.Proxy
import           GHC.TypeLits

newtype JSONCustomOptions n a = JSONCustomOptions a deriving Generic

defaultRecordTypeOptions :: Int -> Options
defaultRecordTypeOptions n = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop n
  , constructorTagModifier = camelTo2 '_' . drop n
  }

instance (Generic a, GToJSON' Value Zero (Rep a),KnownNat n) => ToJSON (JSONCustomOptions n a)
    where
    toJSON (JSONCustomOptions x) = genericToJSON (defaultRecordTypeOptions (nToDrop @n)) x

nToDrop :: forall n. KnownNat n => Int
nToDrop = fromInteger $ natVal (Proxy :: Proxy n)

instance (Generic a, GFromJSON Zero (Rep a), KnownNat n) => FromJSON (JSONCustomOptions n a)
  where
  parseJSON = fmap JSONCustomOptions . genericParseJSON (defaultRecordTypeOptions (nToDrop @n))
