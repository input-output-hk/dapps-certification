{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE DeriveGeneric              #-}

module Plutus.Certification.Metrics where
import Data.Time
import qualified IOHK.Certification.Persistence as DB
import Data.Maybe
import Data.UUID (UUID)
import Data.Swagger
import Data.Aeson
import GHC.Generics

--------------------------------------------------------------------------------
-- | ALIASES

type ExecutionTime = Int
type Start = UTCTime
type End = UTCTime


data RunTimeMetric = RunTimeMetric
  { runId :: !UUID
  , startTime :: !UTCTime
  , endTime :: !UTCTime
  , profileId :: !DB.ProfileId
  , certified :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON RunTimeMetric where
  toJSON = genericToJSON defaultOptions

instance FromJSON RunTimeMetric where
  parseJSON = genericParseJSON defaultOptions

instance ToSchema RunTimeMetric where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

runToMetric :: Start -> End -> DB.Run -> RunTimeMetric
runToMetric start end run = RunTimeMetric
  { runId = run.runId
  , startTime = max start run.created
  , profileId = run.profileId
  , endTime = min end runEnd'
  , certified = isJust run.reportContentId
  }
  where
  runEnd' = fromMaybe run.syncedAt run.finishedAt

-- Calculate the duration between two UTCTimes
duration :: UTCTime -> UTCTime -> NominalDiffTime
duration start end = diffUTCTime end start

type MinimumDuration = NominalDiffTime

longRunning :: MinimumDuration -> [RunTimeMetric] ->  [RunTimeMetric]
longRunning minDuration = filter
  (\entity -> duration (startTime entity) (endTime entity) >= minDuration)
