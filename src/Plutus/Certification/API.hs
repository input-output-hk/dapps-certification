{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Plutus.Certification.API
  ( API
  , NamedAPI(..)
  , VersionV1(..)
  , RunIDV1(..)
  , FlakeRefV1(..)
  , RunStatusV1(..)
  , StepState(..)
  , CertifyingStatus(..)
  , IncompleteRunStatus(..)
  , Cicero.Run.RunLog(..)
  , KnownActionType(..)
  ) where

import Control.Applicative
import Servant.API
import Servant.API.Generic
import Servant.API.Verbs
import Data.Version
import Data.Aeson
import Data.Aeson.Encoding
import Network.URI
import Data.UUID
import Data.ByteString.Lazy.Char8
import Data.Time.LocalTime
import Data.Text hiding (unpack, pack)
import IOHK.Certification.Interface
import qualified IOHK.Cicero.API.Run as Cicero.Run (RunLog(..))

type API = NamedRoutes NamedAPI
data NamedAPI mode = NamedAPI
  { version :: mode :- "version" :> Get '[JSON] VersionV1
  , versionHead :: mode :- "version" :> HeadNoContent
  , createRun :: mode :- "run" :> ReqBody '[PlainText] FlakeRefV1 :> PostCreated '[OctetStream, PlainText, JSON] RunIDV1
  , getRun :: mode :- "run" :> Capture "id" RunIDV1 :> Get '[JSON] RunStatusV1
  , abortRun :: mode :- "run" :> Capture "id" RunIDV1 :> DeleteNoContent
  , getLogs :: mode :- "run"
      :> Capture "id" RunIDV1
      :> "logs"
      :> QueryParam "after" ZonedTime
      :> QueryParam "action-type" KnownActionType
      :> Get '[JSON] [Cicero.Run.RunLog]
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
        s -> Left $ "URI '" ++ uristr ++ "' must be a github: flakeref, not '" ++ s ++ "'"
      Nothing -> Left $ "couldn't not parse '" ++ uristr ++ "' as an absolute URI"
    where
      uristr = unpack uribs

instance MimeRender PlainText FlakeRefV1 where
  mimeRender _ ref = pack $ uriToString id ref.uri ""

newtype RunIDV1 = RunID { uuid :: UUID } deriving newtype (FromHttpApiData, ToHttpApiData, ToJSON)

instance MimeRender PlainText RunIDV1 where
  mimeRender _ rid = toLazyASCIIBytes rid.uuid

instance MimeRender OctetStream RunIDV1 where
  mimeRender _ rid = toByteString rid.uuid

instance MimeUnrender OctetStream RunIDV1 where
  mimeUnrender _ ridbs = case fromByteString ridbs of
    Just rid -> pure $ RunID rid
    Nothing -> Left $ "couldn't parse '" ++ (unpack ridbs) ++ "' as a run ID"

data StepState
  = Running
  | Failed deriving stock (Eq, Ord)

instance ToJSON StepState where
  toJSON Running = String "running"
  toJSON Failed = String "failed"

  toEncoding Running = text "running"
  toEncoding Failed = text "failed"

instance FromJSON StepState where
  parseJSON = withText "StepState" go
    where
      go t
        | t == "running" = pure Running
        | t == "failed" = pure Failed
        | otherwise = fail $ "unknown step state " ++ show t

data CertifyingStatus = CertifyingStatus
  { certifyingState :: !StepState
  , certifyingProgress :: !(Maybe Progress)
  , certifyingPlan :: !(Maybe [CertificationTask])
  }

data IncompleteRunStatus
  = Queued
  | Preparing !StepState
  | Building !StepState
  | Certifying !CertifyingStatus

instance ToJSON IncompleteRunStatus where
  toJSON Queued = object
    [ "status" .= String "queued"
    ]
  toJSON (Preparing s) = object
    [ "status" .= ("preparing" :: Text)
    , "state"  .= s
    ]
  toJSON (Building s) = object
    [ "status" .= ("building" :: Text)
    , "state"  .= s
    ]
  toJSON (Certifying (CertifyingStatus {..})) = object $
      [ "status" .= ("certifying" :: Text)
      , "state"  .= certifyingState
      ] ++ maybeProgress ++ maybePlan
    where
      maybeProgress
        | Just p <- certifyingProgress = [ "progress" .= p ]
        | otherwise = []
      maybePlan
        | Just p <- certifyingPlan = [ "plan" .= p ]
        | otherwise = []

  toEncoding Queued = pairs
    ( "status" .= ("queued" :: Text)
    )
  toEncoding (Preparing s) = pairs
    ( "status" .= ("preparing" :: Text)
   <> "state"  .= s
    )
  toEncoding (Building s) = pairs
    ( "status" .= ("building" :: Text)
   <> "state"  .= s
    )
  toEncoding (Certifying (CertifyingStatus {..})) = pairs
    ( "status" .= ("certifying" :: Text)
   <> "state"  .= certifyingState
   <> (maybe mempty ("progress" .=) certifyingProgress)
   <> (maybe mempty ("plan" .=) certifyingPlan)
    )

instance FromJSON IncompleteRunStatus where
  parseJSON = withObject "IncompleteRunStatus" \o -> do
    status <- o .: "status"
    if | status == ("queued" :: Text) -> pure Queued
       | status == "preparing" -> Preparing <$> o .: "state"
       | status == "building" -> Building <$> o .: "state"
       | status == "certifying" -> Certifying <$> (CertifyingStatus
           <$> o .: "state"
           <*> o .:! "progress"
           <*> o .:! "plan")
       | otherwise -> fail $ "unknown status " ++ show status

data RunStatusV1
  = Incomplete !IncompleteRunStatus
  | Finished !CertificationResult

instance ToJSON RunStatusV1 where
  toJSON (Incomplete i) = toJSON i
  toJSON (Finished v) = object
    [ "status" .= ("finished" :: Text)
    , "result" .= v
    ]

  toEncoding (Incomplete i) = toEncoding i
  toEncoding (Finished v) = pairs
    ( "status" .= ("finished" :: Text)
   <> "result" .= v
    )
instance FromJSON RunStatusV1 where
  parseJSON v = parseIncomplete v <|> parseFinished v
    where
      parseIncomplete v' = Incomplete <$> parseJSON v'
      parseFinished = withObject "RunStatusV1" \o -> do
        status <- o .: "status"
        if status == ("finished" :: Text)
          then Finished <$> o .: "result"
          else fail $ "unknown status " ++ show status
--
-- | Types of Cicero actions we know and care about
data KnownActionType
  = -- | @plutus-certification/generate-flake@
    Generate
  | -- | @plutus-certification/build-flake@
    Build
  | -- | @plutus-certification/run-certify@
    Certify
  deriving stock (Read,Show,Eq,Generic)

instance FromHttpApiData KnownActionType where
  parseUrlPiece "generate" = Right Generate
  parseUrlPiece "build"    = Right Build
  parseUrlPiece "certify"  = Right Certify
  parseUrlPiece _          = Left "Unknown ActionType"

instance ToHttpApiData KnownActionType where
  toUrlPiece Generate = "generate"
  toUrlPiece Build    = "build"
  toUrlPiece Certify  = "certify"


--NOTE: we keep Enum derivation explicitly to nail down the right action order 
-- and also make it future-proof for any data constructors rearrangements
instance Enum KnownActionType where
  fromEnum Generate = 0
  fromEnum Build    = 1
  fromEnum Certify  = 2

  toEnum 0 = Generate
  toEnum 1 = Build
  toEnum 2 = Certify
  toEnum _ = errorWithoutStackTrace "Plutus.Certification.API.KnownActionType.toEnum: bad argument"

instance Ord KnownActionType where
  a <= b = fromEnum a <= fromEnum b
