{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE OverloadedLists            #-}

module IOHK.Certification.Interface where

import GHC.Generics
import Data.Data
import Control.Applicative
import Data.Aeson hiding (Success, Error)
import Data.Aeson.Encoding
import Data.Text as Text hiding (index)
import Data.Swagger
import Data.Char (isAlphaNum)
import Control.Lens hiding ((.=))
import Options.Applicative as Opts hiding (Success, Error)

import qualified Data.Swagger.Lens as SL

-- | Renderable certification task names.
data CertificationTaskName
  = UnitTestsTask
  | StandardPropertyTask
--   | DoubleSatisfactionTask
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
-- renderTask DoubleSatisfactionTask = Right "double-satisfaction"
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
        --    | nm == "double-satisfaction" -> pure DoubleSatisfactionTask
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
  deriving (Data, Eq, Generic)

instance Show GitHubAccessTokenType where
  show PersonalToken       = "ghp"
  show OAuthToken          = "gho"
  show UserToServerToken   = "ghu"
  show ServerToServerToken = "ghs"
  show RefreshToken        = "ghr"

parseGitHubAccessTokenType :: String -> Either String GitHubAccessTokenType
parseGitHubAccessTokenType "ghp" = Right PersonalToken
parseGitHubAccessTokenType "gho" = Right OAuthToken
parseGitHubAccessTokenType "ghu" = Right UserToServerToken
parseGitHubAccessTokenType "ghs" = Right ServerToServerToken
parseGitHubAccessTokenType "ghr" = Right RefreshToken
parseGitHubAccessTokenType _ = Left "invalid access token type"

data GitHubAccessToken = GitHubAccessToken
  { ghAccessTokenPrefix :: GitHubAccessTokenType
  , ghAccessTokenSuffix :: Text
  } deriving (Data, Eq, Generic)

instance ToJSON GitHubAccessToken where
  toJSON = toJSON . show

instance FromJSON GitHubAccessToken where
  parseJSON = withText "GitHubAccessToken" $ \token ->
    case ghAccessTokenFromText token of
      Left err -> fail err
      Right t  -> pure t

ghAccessTokenPattern :: Text
ghAccessTokenPattern = "^gh[oprsu]_[A-Za-z0-9]{36}$"

instance ToSchema GitHubAccessToken where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "GitHubAccessToken") $ mempty
      & type_ ?~ SwaggerString
      & SL.pattern ?~ ghAccessTokenPattern

instance Show GitHubAccessToken where
  -- concat prefx and suffix
  show (GitHubAccessToken pfx sfx) = show pfx ++ "_" ++ unpack sfx

-- | Parse a GitHub access token.
-- The token must be of the form "ghA_XXXXX" where
-- is A is a letter of p,o,u,s or r and XXXXX is a sequence of 36 alphanumeric
ghAccessTokenFromText :: Text -> Either String GitHubAccessToken
ghAccessTokenFromText t = case Text.splitOn "_" t of
  [pfx, sfx] -> do
    pfx' <- parseGitHubAccessTokenType (Text.unpack pfx)
    sfx' <- if Text.length sfx == 36 && Text.all isAlphaNum sfx
      then Right sfx
      else Left "invalid suffix"
    Right $ GitHubAccessToken pfx' sfx'
  _ -> Left "invalid token"

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

data CertOptNumTestsArgs = CertOptNumTestsArgs
                         { numStandardProperty   :: !(Maybe Int)
                         , numNoLockedFunds      :: !(Maybe Int)
                         , numNoLockedFundsLight :: !(Maybe Int)
                         , numCrashTolerance     :: !(Maybe Int)
                         , numWhiteList          :: !(Maybe Int)
                         , numDLTests            :: !(Maybe Int)
                         } deriving (Show, Eq, Generic)

data CertifyArgs = CertifyArgsNumTests CertOptNumTestsArgs
                 | DefaultCertifyArgs
                 deriving (Show, Eq, Generic)

instance FromJSON CertifyArgs where
  parseJSON = withObject "CertifyArgs" \o ->
    (CertifyArgsNumTests <$> o .: "numTests")
   <|> pure DefaultCertifyArgs

instance ToJSON CertifyArgs where
  toJSON (CertifyArgsNumTests n) = object
    [ "numTests" .= n ]
  toJSON DefaultCertifyArgs = object
    [ ]


instance ToSchema CertifyArgs where
  declareNamedSchema _ = do
    certOptNumTestsSchema <- declareSchemaRef (Proxy :: Proxy CertOptNumTestsArgs)
    return $ NamedSchema (Just "CertifyArgs") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("certOptNumTests", certOptNumTestsSchema) ]

instance FromJSON CertOptNumTestsArgs where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON CertOptNumTestsArgs where
  toJSON = genericToJSON defaultOptions

instance ToSchema CertOptNumTestsArgs where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

parseCertifyArgs :: Parser CertifyArgs
parseCertifyArgs =
  customOptionsParser <|> pure DefaultCertifyArgs

customOptionsParser :: Parser CertifyArgs
customOptionsParser  = hsubparser
  (command "custom-options" customCertifyArgsInfo)

customCertifyArgsInfo :: ParserInfo CertifyArgs
customCertifyArgsInfo = Opts.info parseCustomCertifyArgs
  ( fullDesc
 <> header "custom-options — to run the certification process with custom options"
  )

parseCustomCertifyArgs :: Parser CertifyArgs
parseCustomCertifyArgs = CertifyArgsNumTests <$> certOptNumTestsParser


numTestsParser :: Parser Int
numTestsParser = option auto
      ( long "num-tests"
     <> metavar "NUM_TESTS"
     <> help "Number of tests to run"
      )
certOptNumTestsParser :: Parser CertOptNumTestsArgs
certOptNumTestsParser = CertOptNumTestsArgs
    <$> optional (option auto
          ( long "num-standard-property"
         <> metavar "NUM_STANDARD_PROPERTY"
         <> help "Number of tests to run for standard property"
          ))
    <*> optional (option auto
          ( long "num-no-locked-funds"
         <> metavar "NUM_NO_LOCKED_FUNDS"
         <> help "Number of tests to run for no locked funds property"
          ))
    <*> optional (option auto
          ( long "num-no-locked-funds-light"
         <> metavar "NUM_NO_LOCKED_FUNDS_LIGHT"
         <> help "Number of tests to run for no locked funds light property"
          ))
    <*> optional (option auto
          ( long "num-crash-tolerance"
         <> metavar "NUM_CRASH_TOLERANCE"
         <> help "Number of tests to run for crash tolerance property"
          ))
    <*> optional (option auto
          ( long "num-whitelist"
         <> metavar "NUM_WHITELIST"
         <> help "Number of tests to run for whitelist property"
          ))
    <*> optional (option auto
          ( long "num-dl-tests"
         <> metavar "NUM_DL_TESTS"
         <> help "Number of tests to run for DL tests"
          ))

certifyArgsToCommandList :: CertifyArgs -> [String]
certifyArgsToCommandList (CertifyArgsNumTests CertOptNumTestsArgs{..}) =
    -- remove empty lines
    Prelude.filter (not . Prelude.null) allLines
    where
    allLines =
      [ "custom-options" ]
      ++ maybe [] (\n -> ["--num-standard-property",show n]) numStandardProperty
      ++ maybe [] (\n -> ["--num-no-locked-funds",show n]) numNoLockedFunds
      ++ maybe [] (\n -> ["--num-no-locked-funds-light",show n]) numNoLockedFundsLight
      ++ maybe [] (\n -> ["--num-crash-tolerance",show n]) numCrashTolerance
      ++ maybe [] (\n -> ["--num-whitelist",show n]) numWhiteList
      ++ maybe [] (\n -> ["--num-dl-tests",show n]) numDLTests

certifyArgsToCommandList DefaultCertifyArgs = []
