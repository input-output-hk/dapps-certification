{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Plutus.Certification.API.RunLog where

import Data.Aeson
import Data.Swagger
import Data.Text
import Data.Time.LocalTime

data RunLog = RunLog
  { time :: !ZonedTime
  , source :: !Text
  , text :: !Text
  }

instance FromJSON RunLog where
  parseJSON = withObject "RunLog" \o -> RunLog
    <$> o .: "Time"
    <*> o .: "Source"
    <*> o .: "Text"

instance ToJSON RunLog where
  toJSON r = object
    [ "Time" .= r.time
    , "Source" .= r.source
    , "Text" .= r.text
    ]
  toEncoding r = pairs
    ( "Time" .= r.time
   <> "Source" .= r.source
   <> "Text" .= r.text
    )

instance ToSchema RunLog where
  --TODO: find a way to embed aeson Value to the definition
  declareNamedSchema _  = pure $ NamedSchema (Just "RunLog") mempty
