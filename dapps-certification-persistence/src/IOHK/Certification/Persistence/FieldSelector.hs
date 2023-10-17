{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module IOHK.Certification.Persistence.FieldSelector (fields,fieldNames) where
import Data.Data
import GHC.Generics
import Data.String (IsString(..))
import Data.Bifunctor (Bifunctor(first))


class Selectors rep where
  selectors :: Proxy rep -> [(String, TypeRep)]

instance Selectors f => Selectors (M1 D x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors _ =
    [ ( selName (undefined :: M1 S s (K1 R t) ()) , typeRep (Proxy :: Proxy t) ) ]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors _ = selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b)

instance Selectors U1 where
  selectors _ = []

fields :: forall a s. (Generic a, Selectors (Rep a),IsString s)
           => [(s, TypeRep)]
fields = first fromString <$> selectors (Proxy :: Proxy (Rep a))

fieldNames :: forall a s. (Generic a, Selectors (Rep a),IsString s) => [s]
fieldNames = fst <$> fields @a @s

{-
>>> :set -XDeriveGeneric
>>> import Data.Int

>>> data Record = Record { recordId :: Int32, recordName :: String } deriving (Generic)
>>> selectors (Proxy :: Proxy (Rep Record))
>>> fieldNames @Record @String
[("recordId",Int32),("recordName",[Char])]
["recordId","recordName"]
-}
