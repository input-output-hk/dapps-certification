{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveGeneric #-}
module IOHK.Certification.Interface where

import GHC.Generics
import Control.Applicative
import Data.Aeson hiding (Success, Error)
import Data.Aeson.Encoding
import Data.Text as Text hiding (index)
import Data.Swagger
import Data.Char (isAlphaNum)

-- | Renderable certification task names.
data CertificationTaskName
  = UnitTestsTask
  | StandardPropertyTask
  | DoubleSatisfactionTask
  | NoLockedFundsTask
  | NoLockedFundsLightTask
  | CrashToleranceTask
  | WhitelistTask
  | DLTestsTask
  | UnknownTask !String
  deriving (Generic)

instance ToSchema CertificationTaskName where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

-- | Render known names or 'show' unknown ones.
renderTask :: CertificationTaskName -> Either String Text
renderTask UnitTestsTask = Right "unit-tests"
renderTask StandardPropertyTask = Right "standard-property"
renderTask DoubleSatisfactionTask = Right "double-satisfaction"
renderTask NoLockedFundsTask = Right "no-locked-funds"
renderTask NoLockedFundsLightTask = Right "no-locked-funds-light"
renderTask CrashToleranceTask = Right "crash-tolerance"
renderTask WhitelistTask = Right "white-list"
renderTask DLTestsTask = Right "dl-tests"
renderTask (UnknownTask nm) = Left nm

instance ToJSON CertificationTaskName where
  toJSON t = case renderTask t of
    Left nm -> object [ "unknown-name" .= nm ]
    Right nm -> String nm
  toEncoding t = case renderTask t of
    Left nm -> pairs ( "unknown-name" .= nm )
    Right nm -> text nm

instance FromJSON CertificationTaskName where
  parseJSON v = unknown <|> known
    where
      unknown = flip (withObject "CertificationTaskName") v \o ->
        UnknownTask <$> o .: "unknown-name"
      known = flip (withText "CertificationTaskName") v \nm ->
        if | nm == "unit-tests" -> pure UnitTestsTask
           | nm == "standard-property" -> pure StandardPropertyTask
           | nm == "double-satisfaction" -> pure DoubleSatisfactionTask
           | nm == "no-locked-funds" -> pure NoLockedFundsTask
           | nm == "no-locked-funds-light" -> pure NoLockedFundsLightTask
           | nm == "crash-tolerance" -> pure CrashToleranceTask
           | nm == "white-list" -> pure WhitelistTask
           | nm == "dl-tests" -> pure DLTestsTask
           | otherwise -> fail $ "unknown task name " ++ unpack nm

-- | A plutus-apps independent representation of a certification task
data CertificationTask = CertificationTask
  { name :: !CertificationTaskName
  , index :: !Int
  } deriving (Generic)

instance ToSchema CertificationTask

instance ToJSON CertificationTask where
  toJSON CertificationTask {..} = object
    [ "name" .= name
    , "index" .= index
    ]
  toEncoding CertificationTask {..} = pairs
    ( "name" .= name
   <> "index" .= index
    )

instance FromJSON CertificationTask where
  parseJSON = withObject "CertificationTask" \o -> CertificationTask
    <$> o .: "name"
    <*> o .: "index"

-- | Progress in a quickcheck run
data QCProgress = QCProgress
  { qcSuccesses :: !Integer
  , qcFailures :: !Integer
  , qcDiscarded :: !Integer
  , qcExpected :: !(Maybe Int)
  } deriving Generic

instance ToSchema QCProgress

instance ToJSON QCProgress where
  toJSON QCProgress {..} = object $
    [ "successes" .= qcSuccesses
    , "failures" .= qcFailures
    , "discarded" .= qcDiscarded
    ] <> (maybe mempty (pure . ("expected" .=)) qcExpected)
  toEncoding QCProgress {..} = pairs
    ( "successes" .= qcSuccesses
   <> "failures" .= qcFailures
   <> "discarded" .= qcDiscarded
   <> (maybe mempty ("expected" .=) qcExpected)
    )

instance FromJSON QCProgress where
  parseJSON = withObject "QCProgress" \o -> QCProgress
    <$> o .: "successes"
    <*> o .: "failures"
    <*> o .: "discarded"
    <*> o .:! "expected"

-- | The result of a certification task.
data TaskResult = TaskResult
  { task :: !CertificationTask
  , qcResult :: !QCProgress
  , succeeded :: !Bool
  } deriving Generic

data GitHubAccessTokenType
  = PersonalToken
  | OAuthToken
  | UserToServerToken
  | ServerToServerToken
  | RefreshToken
  deriving (Eq, Generic)
data GitHubAccessToken = GitHubAccessToken
  { ghAccessTokenPrefix :: GitHubAccessTokenType
  , ghAccessTokenSuffix :: Text
  } deriving (Eq, Generic)

-- | Parse a GitHub access token.
-- The token must be of the form "ghA_XXXXX" where
-- is A is a letter of p,o,u,s or r and XXXXX is a sequence of 36 alphanumeric
ghAccessTokenFromText :: Text -> Either String GitHubAccessToken
ghAccessTokenFromText t = case Text.splitOn "_" t of
  [pfx, sfx] -> do
    pfx' <- case pfx of
      "ghp" -> Right PersonalToken
      "gho" -> Right OAuthToken
      "ghu" -> Right UserToServerToken
      "ghs" -> Right ServerToServerToken
      "ghr" -> Right RefreshToken
      _ -> Left "invalid prefix"
    sfx' <- if Text.length sfx == 36 && Text.all isAlphaNum sfx
      then Right sfx
      else Left "invalid suffix"
    Right $ GitHubAccessToken pfx' sfx'
  _ -> Left "invalid token"

-- | Parse a GitHub access token without error handling.
-- see 'ghAccessTokenFromText' for details.
-- This function is unsafe because it can throw an error
-- if the token is not known to be valid.
-- To be used only when the token is known to be valid (eg. from the database or a config file)
knownGhAccessTokenFromText :: Text -> GitHubAccessToken
knownGhAccessTokenFromText t = case ghAccessTokenFromText t of
  Left err -> error err
  Right t' -> t'

-- | Render a GitHub access token.
ghAccessTokenToText :: GitHubAccessToken -> Text
ghAccessTokenToText (GitHubAccessToken t s) =
  let prefix' = case t of
        PersonalToken       -> "ghp"
        OAuthToken          -> "gho"
        UserToServerToken   -> "ghu"
        ServerToServerToken -> "ghs"
        RefreshToken        -> "ghr"
  in prefix' <> "_" <> s


instance ToSchema TaskResult

instance ToJSON TaskResult where
  toJSON TaskResult {..} = object
    [ "task" .= task
    , "qc-result" .= qcResult
    , "succeeded" .= succeeded
    ]
  toEncoding TaskResult {..} = pairs
    ( "task" .= task
   <> "qc-result" .= qcResult
   <> "succeeded" .= succeeded
    )

instance FromJSON TaskResult where
  parseJSON = withObject "TaskResult" \o -> TaskResult
    <$> o .: "task"
    <*> o .: "qc-result"
    <*> o .: "succeeded"

-- | Total progress in a certification run.
data Progress = Progress
  { currentTask :: !(Maybe CertificationTask)
  , currentQc :: !QCProgress
  , finishedTasks :: ![TaskResult]
  , progressIndex :: !Integer
  } deriving (Generic)
instance ToSchema Progress

instance ToJSON Progress where
  toJSON Progress {..} = object
    [ "current-task" .= currentTask
    , "qc-progress" .= currentQc
    , "finished-tasks" .= finishedTasks
    , "progress-index" .= progressIndex
    ]
  toEncoding Progress {..} = pairs
    ( "current-task" .= currentTask
   <> "qc-progress" .= currentQc
   <> "finished-tasks" .= finishedTasks
   <> "progress-index" .= progressIndex
    )
instance FromJSON Progress where
  parseJSON = withObject "Progress" \o -> Progress
    <$> o .: "current-task"
    <*> o .: "qc-progress"
    <*> o .: "finished-tasks"
    <*> o .: "progress-index"

-- | A plutus-apps independent representation of an entire certification run
data CertificationResult = forall a . (ToJSON a) => CertificationResult !a

instance ToSchema CertificationResult where
  --TODO: find a way to embed aeson Value to the definition
  declareNamedSchema _  =
    return $ NamedSchema (Just "CertificationResult") $ mempty

instance ToJSON CertificationResult where
  toJSON (CertificationResult v) = toJSON v
  toEncoding (CertificationResult v) = toEncoding v

instance FromJSON CertificationResult where
  parseJSON v = CertificationResult <$> pure v

-- | A message from the certification process
data Message
  = Status !Progress
  | Success !CertificationResult
  | Plan ![CertificationTask]

instance ToJSON Message where
  toJSON (Status p) = object
    [ "status" .= p
    ]
  toJSON (Success c) = object
    [ "success" .= c
    ]
  toJSON (Plan tasks) = object
    [ "plan" .= tasks
    ]
  toEncoding (Status p) = pairs
    ( "status" .= p
    )
  toEncoding (Success c) = pairs
    ( "success" .= c
    )
  toEncoding (Plan tasks) = pairs
    ( "plan" .= tasks
    )
instance FromJSON Message where
  parseJSON = withObject "Message" \o ->
      success o <|> stat o <|> plan o
    where
      success o = Success <$> o .: "success"
      stat o = Status <$> o .: "status"
      plan o = Plan <$> o .: "plan"
