{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Plutus.Certification.API
  ( API
  , NamedAPI(..)
  , VersionV1(..)
  , RunIDV1(..)
  , FlakeRefV1(..)
  ) where

import Servant.API
import Servant.API.NamedRoutes
import Servant.API.Generic
import Servant.API.Verbs
import Data.Version
import Data.Aeson
import Network.URI
import Data.UUID
import Data.ByteString.Lazy.Char8

type API = NamedRoutes NamedAPI
data NamedAPI mode = NamedAPI
  { version :: mode :- "version" :> Get '[JSON] VersionV1
  , versionHead :: mode :- "version" :> HeadNoContent
  , createRun :: mode :- "run" :> ReqBody '[PlainText] FlakeRefV1 :> PostCreated '[OctetStream, PlainText] RunIDV1
  } deriving stock Generic

newtype VersionV1 = VersionV1 { version :: Version }

versionV1Branch :: VersionV1 -> [Int]
versionV1Branch v = versionBranch $ v.version

instance ToJSON VersionV1 where
  toEncoding = toEncoding . versionV1Branch
  toJSON = toJSON . versionV1Branch

instance FromJSON VersionV1 where
  parseJSON v = VersionV1 . makeVersion <$> parseJSON v

newtype FlakeRefV1 = FlakeRef { uri :: URI }

instance MimeUnrender PlainText FlakeRefV1 where
  mimeUnrender _ uribs = case parseAbsoluteURI uristr of
      Just u -> case u.uriScheme of
        "github:" -> Right $ FlakeRef u
        scheme -> Left $ "URI '" ++ uristr ++ "' must be a github: flakeref, not '" ++ scheme ++ "'"
      Nothing -> Left $ "couldn't not parse '" ++ uristr ++ "' as an absolute URI"
    where
      uristr = unpack uribs

newtype RunIDV1 = RunID { uuid :: UUID }

instance MimeRender PlainText RunIDV1 where
  mimeRender _ rid = toLazyASCIIBytes rid.uuid

instance MimeRender OctetStream RunIDV1 where
  mimeRender _ rid = toByteString rid.uuid
