{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Plutus.Certification.API
import Servant.Client hiding (manager)
import Servant.Client.Core.BaseUrl
import Servant.Client.Core
import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Header
import Network.HTTP.Client.TLS
import Options.Applicative
import Control.Exception hiding (handle)
import Data.UUID as UUID
import Data.ByteString.Char8 as BS hiding (hPutStrLn)
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.Coerce
import Network.URI hiding (scheme)
import Servant.API hiding (addHeader)
import Data.Aeson
import System.IO (stdout)
import Data.Time.LocalTime
import Data.Time
import Data.Text as Text


type PublicKey = ByteString

flakeRefReader :: ReadM FlakeRefV1
flakeRefReader = do
  urlStr <- str
  case parseAbsoluteURI urlStr of
    Just u -> case u.uriScheme of
      "github:" -> pure $ FlakeRef u
      scheme -> readerError $ "URI '" ++ urlStr ++ "' must be a github: flakeref, not '" ++ scheme ++ "'"
    Nothing -> readerError $ "couldn't not parse '" ++ urlStr ++ "' as an absolute URI"

createRunParser :: Parser CreateRunArgs
createRunParser = CreateRunArgs
  <$> argument flakeRefReader
    ( metavar "REF"
   <> help "the flake reference pointing to the repo to build"
    )
  <*> publicKeyParser

createRunInfo :: ParserInfo CreateRunArgs
createRunInfo = info createRunParser
  ( fullDesc
 <> header "plutus-certification-client run create — Create a new testing run"
  )

getRunParser :: Parser RunIDV1
getRunParser = argument (maybeReader (coerce . UUID.fromString))
  ( metavar "RUN_ID"
 <> help "the ID of the run"
  )

publicKeyParser :: Parser PublicKey
publicKeyParser = option str
  ( long "public-key"
 <> metavar "PUB_KEY"
 <> help "wallet public Key"
  )

getRunInfo :: ParserInfo RunIDV1
getRunInfo = info getRunParser
  ( fullDesc
 <> header "plutus-certification-client run get — Get the status of a run"
  )

abortRunInfo :: ParserInfo AbortRunArgs
abortRunInfo = info abortRunParser
  ( fullDesc
 <> header "plutus-certification-client run abort — Abort a run"
  )

getLogsParser :: Parser GetLogsArgs
getLogsParser = GetLogsArgs
  <$> getRunParser
  <*> optional (option generalReader
        ( long "after"
       <> metavar "AFTER"
       <> help "getting all the logs following a certain timestamp"
        ))
  <*> optional (option auto
        ( long "action-type"
       <> metavar "TYPE"
       <> help "filter logs by action-type (Generate/Build/Certify)"
        ))

getRunsParser :: Parser GetRunsArgs
getRunsParser = GetRunsArgs
  <$> publicKeyParser
  <*> optional (option generalReader
        ( long "after"
       <> metavar "AFTER"
       <> help "getting all the runs following a certain timestamp"
        ))
  <*> optional (option auto
        ( long "max-count"
       <> metavar "COUNT"
       <> help "maximum number or runs to fetch"
        ))

abortRunParser :: Parser AbortRunArgs
abortRunParser = AbortRunArgs
  <$> getRunParser
  <*> publicKeyParser

generalReader :: FromHttpApiData a => ReadM a
generalReader = do
  urlStr <- str
  case parseUrlPiece urlStr of
    Right u -> pure u
    Left t -> readerError $ Text.unpack t

getLogsInfo :: ParserInfo GetLogsArgs
getLogsInfo = info getLogsParser
  ( fullDesc
 <> header "plutus-certification-client run get-logs — Get the logs of a run"
  )

getRunsInfo :: ParserInfo GetRunsArgs
getRunsInfo = info getRunsParser
  ( fullDesc
 <> header "plutus-certification-client run get-many — Get many runs"
  )

getCertificationInfo :: ParserInfo RunIDV1
getCertificationInfo = info getRunParser
  ( fullDesc
 <> header "plutus-certification-client run get-certification — Get the attached certification info of a run"
  )

createCertificationInfo :: ParserInfo CreateCertificationArgs
createCertificationInfo = info createCertificationParser
  ( fullDesc
 <> header ( "plutus-certification-client run create-certification — Generates the certification objects of a run."
      ++ " It works only if the run succeeded and no certification was already generated before" )
  )

createCertificationParser :: Parser CreateCertificationArgs
createCertificationParser = CreateCertificationArgs
  <$> getRunParser
  <*> publicKeyParser

data RunCommand
  = Create !CreateRunArgs
  | Get !RunIDV1
  | Abort !AbortRunArgs
  | GetLogs !GetLogsArgs
  | GetRuns !GetRunsArgs
  | GetCertification !RunIDV1
  | CreateCertification !CreateCertificationArgs

runCommandParser :: Parser RunCommand
runCommandParser = hsubparser
  ( command "create" (Create <$> createRunInfo)
 <> command "get" (Get <$> getRunInfo)
 <> command "abort" (Abort <$> abortRunInfo)
 <> command "get-logs" (GetLogs <$> getLogsInfo)
 <> command "get-many" (GetRuns <$> getRunsInfo)
 <> command "get-certification" (GetCertification <$> getCertificationInfo)
 <> command "create-certification" (CreateCertification <$> createCertificationInfo)
  )

data CreateRunArgs = CreateRunArgs !FlakeRefV1 !PublicKey

data GetRunsArgs = GetRunsArgs !PublicKey !(Maybe UTCTime) !(Maybe Int)

data AbortRunArgs = AbortRunArgs !RunIDV1 !PublicKey
data CreateCertificationArgs= CreateCertificationArgs !RunIDV1 !PublicKey

data GetLogsArgs = GetLogsArgs
  { runId :: !RunIDV1
  , after :: !(Maybe ZonedTime)
  , actionType :: !(Maybe KnownActionType)
  }

runCommandInfo :: ParserInfo RunCommand
runCommandInfo = info runCommandParser
  ( fullDesc
 <> header "plutus-certification-client run — Manage certification runs"
  )

currentProfileInfo :: ParserInfo ProfileCommand
currentProfileInfo = info currentProfileParser
  ( fullDesc
 <> header "plutus-certification-client profile Current profile Management"
  )

currentProfileParser :: Parser ProfileCommand
currentProfileParser = hsubparser
  ( command "get" (GetCurrentProfile <$> getCurrentProfileInfo)
 <> command "update" (UpdateCurrentProfile <$> updateCurrentProfileInfo)
  )

updateCurrentProfileInfo :: ParserInfo UpdateCurrentProfileArgs
updateCurrentProfileInfo = info updateCurrentProfileInfoParser
  ( fullDesc
 <> header "plutus-certification-client profile update — Update the current profile information"
  )

updateCurrentProfileInfoParser :: Parser UpdateCurrentProfileArgs
updateCurrentProfileInfoParser = UpdateCurrentProfileArgs
  <$> publicKeyParser
  <*> profileBodyParser

profileBodyParser :: Parser ProfileBody
profileBodyParser = ProfileBody
  <$> optional (option str
        ( long "dapp"
       <> metavar "DAPP-NAME"
       <> help "dapp identification"
        ))
  <*> optional (option str
        ( long "website"
       <> metavar "WEBSITE"
       <> help "dapp website url"
        ))
  <*> optional (option str
        ( long "vendor"
       <> metavar "VENDOR"
       <> help "vendor identification"
        ))
  <*> optional (option str
        ( long "twitter"
       <> metavar "TWITTER"
       <> help "twitter account"
        ))
  <*> optional (option str
        ( long "linkedin"
       <> metavar "LINKEDIN"
       <> help "linkedin account"
        ))

getCurrentProfileInfo :: ParserInfo PublicKey
getCurrentProfileInfo = info publicKeyParser
  ( fullDesc
 <> header "plutus-certification-client profile get — Get the current profile information"
  )

versionCommandInfo :: ParserInfo ()
versionCommandInfo = info (pure ())
  ( fullDesc
 <> header "plutus-certification-client version — Get the version of the server"
  )

data Command
  = CmdRun !RunCommand
  | CmdVersion
  | CmdCurrentProfile !ProfileCommand

data ProfileCommand
    = GetCurrentProfile !PublicKey
    | UpdateCurrentProfile !UpdateCurrentProfileArgs

data UpdateCurrentProfileArgs = UpdateCurrentProfileArgs PublicKey ProfileBody

commandParser :: Parser Command
commandParser = hsubparser
  ( command "run" (CmdRun <$> runCommandInfo)
 <> command "version" (const CmdVersion <$> versionCommandInfo)
 <> command "profile" (CmdCurrentProfile <$> currentProfileInfo)
  )

data Args = Args
  { certificationURL :: !BaseUrl
  , certificationUser :: !(Maybe PublicKey)
  , cmd :: !Command
  }

baseUrlReader :: ReadM BaseUrl
baseUrlReader = do
  urlStr <- str
  case parseBaseUrl urlStr of
    Left e -> case fromException e of
      Just (InvalidBaseUrlException s) -> readerError $ "invalid URL '" ++ urlStr ++ "': " ++ s
      Nothing -> readerError $ "exception parsing '" ++ urlStr ++ "' as a URL: " ++ displayException e
    Right b -> pure b

argsParser :: Parser Args
argsParser = Args
  <$> option baseUrlReader
        ( long "certification-url"
       <> metavar "CERTIFICATION_URL"
       <> help "URL of the certification server"
       <> showDefaultWith showBaseUrl
       <> (value $ BaseUrl Https "testing.dapps.iog.io" 443 "")
        )
  <*> (optional $ option str
        ( long "user"
       <> metavar "USER"
       <> help "User name for BASIC authentication with the certification server"
        ))
  <*> commandParser

argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
 <> header "plutus-certification-cli — A tool for interacting with the Plutus Certification service"
  )
addAuth :: PublicKey -> AuthenticatedRequest (AuthProtect "public-key")
addAuth = flip mkAuthenticatedRequest (\v -> addHeader hAuthorization (BS.unpack v))

type instance AuthClientData (AuthProtect "public-key") = PublicKey

main :: IO ()
main = do
  args <- execParser argsInfo
  manager <- newTlsManagerWith $ tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  let apiClient = client $ Proxy @API
      cEnv = mkClientEnv manager args.certificationURL
      handle :: (ToJSON a) => ClientM a -> IO ()
      handle c = runClientM c cEnv >>= either throwIO (hPutStrLn stdout . encode)
  case args.cmd of
    CmdVersion ->
      handle $ apiClient.version
    CmdRun (Create (CreateRunArgs ref pubKey)) ->
      handle $ apiClient.createRun (addAuth pubKey) ref
    CmdRun (Get ref) ->
      handle $ apiClient.getRun ref
    CmdRun (Abort (AbortRunArgs ref pubKey)) ->
      handle $ (const True <$> apiClient.abortRun (addAuth pubKey) ref)
    --TODO: investigate why ZonedTime doesn't serialize properly
    CmdRun (GetLogs (GetLogsArgs ref zt act)) ->
      handle $ apiClient.getLogs ref zt act
    CmdRun (GetRuns (GetRunsArgs pubKey after' count')) ->
      handle $ apiClient.getRuns (addAuth pubKey) after' count'
    CmdRun (GetCertification ref) ->
      handle $ apiClient.getCertification ref
    CmdRun (CreateCertification (CreateCertificationArgs ref pubKey)) ->
      handle $ apiClient.createCertification (addAuth pubKey) ref
    CmdCurrentProfile (GetCurrentProfile pubKey) ->
      handle $ apiClient.getCurrentProfile (addAuth pubKey)
    CmdCurrentProfile (UpdateCurrentProfile (UpdateCurrentProfileArgs pubKey profileBody)) ->
      handle $ apiClient.updateCurrentProfile (addAuth pubKey) profileBody
