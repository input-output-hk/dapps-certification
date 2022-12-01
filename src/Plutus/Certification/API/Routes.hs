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

module Plutus.Certification.API.Routes where

import Control.Applicative
import Servant.API as Servant
import Servant.API.Generic
import Servant.API.Verbs
import Data.Version
import Data.Aeson
import Data.Aeson.Encoding
import Network.URI
import Data.UUID
import Data.ByteString.Lazy.Char8 as BSL8
import Data.Time.LocalTime
import Data.Text hiding (unpack, pack)
import Data.Swagger
import IOHK.Certification.Interface
import qualified IOHK.Certification.Persistence as DB
import qualified IOHK.Cicero.API.Run as Cicero.Run (RunLog(..))
import qualified Control.Lens as L
import Data.Time

type API = NamedRoutes NamedAPI

type VersionRoute = "version"
  :> Description "Get the api version"
  :> Get '[JSON] VersionV1

type VersionHeadRoute = "version"
  :> Description "Get the api version (Response Headers only)"
  :> HeadNoContent

type CreateRunRoute = "run"
  :> Description "Create a new testing run"
  :> AuthProtect "public-key"
  :> ReqBody '[PlainText] FlakeRefV1
  :> PostCreated '[OctetStream, PlainText, JSON] RunIDV1

type GetRunRoute = "run"
  :> Description "Get the status of a run"
  :> Capture "id" RunIDV1
  :> Get '[JSON] RunStatusV1

type AbortRunRoute = "run"
  :> Description "Abort a run"
  :> AuthProtect "public-key"
  :> Capture "id" RunIDV1
  :> DeleteNoContent

type GetLogsRoute = "run"
  :> Description "Get the logs of a run"
  :> Capture "id" RunIDV1
  :> "logs"
  :> QueryParam "after" ZonedTime
  :> QueryParam "action-type" KnownActionType
  :> Get '[JSON] [Cicero.Run.RunLog]

type GetRunsRoute = "run"
  :> Description "Query through multiple runs belonging to the profile identified by the auth-key"
  :> AuthProtect "public-key"
  :> QueryParam "after" UTCTime
  :> QueryParam "count" Int
  :> Get '[JSON] [DB.Run]

type GetCurrentProfileRoute = "profile"
  :> Description "Get the current profile information"
  :> "current"
  :> AuthProtect "public-key"
  :> Get '[JSON] DB.Profile

type UpdateCurrentProfileRoute = "profile"
  :> Description "Update the current profile information"
  :> "current"
  :> AuthProtect "public-key"
  :> ReqBody '[JSON] ProfileBody
  :> Put '[JSON] DB.Profile

data NamedAPI mode = NamedAPI
  { version :: mode :- VersionRoute
  , versionHead :: mode :- VersionHeadRoute
  , createRun :: mode :- CreateRunRoute
  , getRun :: mode :- GetRunRoute
  , abortRun :: mode :- AbortRunRoute
  , getLogs :: mode :- GetLogsRoute
  , getRuns :: mode :- GetRunsRoute
  , getCurrentProfile :: mode :- GetCurrentProfileRoute
  , updateCurrentProfile :: mode :- UpdateCurrentProfileRoute
  } deriving stock Generic

data ProfileBody = ProfileBody
   { dapp :: !(Maybe Text)
   , website :: !(Maybe Text)
   , vendor :: !(Maybe Text)
   , twitter :: !(Maybe Text)
   , linkedin :: !(Maybe Text)
   } deriving stock Generic

instance FromJSON ProfileBody where
    parseJSON = withObject "Profile" $ \v -> ProfileBody
      <$> v .:? "dapp"  .!= Nothing
      <*> v .:? "website"  .!= Nothing
      <*> v .:? "vendor"  .!= Nothing
      <*> v .:? "twitter"  .!= Nothing
      <*> v .:? "linkedin"  .!= Nothing

instance ToJSON ProfileBody where

newtype VersionV1 = VersionV1 { version :: Version } deriving (Generic)

versionV1Branch :: VersionV1 -> [Int]
versionV1Branch v = versionBranch $ v.version

instance ToJSON VersionV1 where
  toEncoding = toEncoding . versionV1Branch
  toJSON = toJSON . versionV1Branch

instance FromJSON VersionV1 where
  parseJSON v = VersionV1 . makeVersion <$> parseJSON v

newtype FlakeRefV1 = FlakeRef { uri :: URI }
                   deriving(Generic)

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

newtype RunIDV1 = RunID { uuid :: UUID }
  deriving newtype (FromHttpApiData, ToHttpApiData, ToJSON )
  deriving stock (Generic)

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
  deriving (Generic)

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
  } deriving (Generic)

data IncompleteRunStatus
  = Queued
  | Preparing !StepState
  | Building !StepState
  | Certifying !CertifyingStatus
  deriving (Generic)

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
  deriving (Generic)

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

instance ToSchema VersionV1
instance ToSchema RunStatusV1
instance ToSchema StepState
instance ToSchema CertifyingStatus
instance ToSchema RunIDV1
instance ToParamSchema RunIDV1
instance ToParamSchema KnownActionType
instance ToSchema ProfileBody

instance ToSchema FlakeRefV1  where
   declareNamedSchema _ = do
    return $ NamedSchema (Just "FlakeRefV1") $ mempty
      L.& type_ L.?~ SwaggerString

instance ToSchema Cicero.Run.RunLog where
  --TODO: find a way to embed aeson Value to the definition
  declareNamedSchema _  = pure $ NamedSchema (Just "RunLog") $ mempty
instance ToSchema IncompleteRunStatus where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

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
