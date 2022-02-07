{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Plutus.Certification.API
  ( API
  , NamedAPI(..)
  , VersionV1(..)
  ) where

import Servant.API
import Servant.API.NamedRoutes
import Servant.API.Generic
import Servant.API.Verbs
import Data.Version
import Data.Aeson

type API = NamedRoutes NamedAPI
data NamedAPI mode = NamedAPI
  { version :: mode :- "version" :> Get '[JSON] VersionV1
  , versionHead :: mode :- "version" :> HeadNoContent
  } deriving (Generic)

newtype VersionV1 = VersionV1 { toVersion :: Version }

versionV1Branch :: VersionV1 -> [Int]
versionV1Branch = versionBranch . toVersion

instance ToJSON VersionV1 where
  toEncoding = toEncoding . versionV1Branch
  toJSON = toJSON . versionV1Branch

instance FromJSON VersionV1 where
  parseJSON v = VersionV1 . makeVersion <$> parseJSON v
