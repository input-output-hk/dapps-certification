{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Plutus.Certification.API
import Servant.Client hiding (manager)
import Servant.Client.Core.BaseUrl
import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Options.Applicative
import Control.Exception hiding (handle)
import System.Console.Haskeline
import Data.UUID as UUID
import Data.ByteString.Char8 as BS hiding (hPutStrLn)
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.Coerce
import Network.URI hiding (scheme)
import Servant.API.BasicAuth
import Servant.API
import Servant.Client.Core.BasicAuth
import Data.Aeson
import System.IO (stdout)
import Data.Time.LocalTime
import Data.Text as Text

flakeRefReader :: ReadM FlakeRefV1
flakeRefReader = do
  urlStr <- str
  case parseAbsoluteURI urlStr of
    Just u -> case u.uriScheme of
      "github:" -> pure $ FlakeRef u
      scheme -> readerError $ "URI '" ++ urlStr ++ "' must be a github: flakeref, not '" ++ scheme ++ "'"
    Nothing -> readerError $ "couldn't not parse '" ++ urlStr ++ "' as an absolute URI"

createRunParser :: Parser FlakeRefV1
createRunParser = argument flakeRefReader
  ( metavar "REF"
 <> help "the flake reference pointing to the repo to build"
  )

createRunInfo :: ParserInfo FlakeRefV1
createRunInfo = info createRunParser
  ( fullDesc
 <> header "plutus-certification-client run create — Create a new testing run"
  )

getRunParser :: Parser RunIDV1
getRunParser = argument (maybeReader (coerce . UUID.fromString))
  ( metavar "RUN_ID"
 <> help "the ID of the run"
  )

getRunInfo :: ParserInfo RunIDV1
getRunInfo = info getRunParser
  ( fullDesc
 <> header "plutus-certification-client run get — Get the status of a run"
  )

abortRunInfo :: ParserInfo RunIDV1
abortRunInfo = info getRunParser
  ( fullDesc
 <> header "plutus-certification-client run abort — Abort a run"
  )

getLogsParser :: Parser GetLogsArgs
getLogsParser = GetLogsArgs
  <$> getRunParser
  <*> optional (option zonedTimeReader
        ( long "after"
       <> metavar "AFTER"
       <> help "getting all the logs following a certain timestamp"
        ))
  <*> optional (option auto
        ( long "action-type"
       <> metavar "TYPE"
       <> help "filter logs by action-type (Generate/Build/Certify)"
        ))

zonedTimeReader :: ReadM ZonedTime
zonedTimeReader = do
  urlStr <- str
  case parseUrlPiece urlStr of
    Right u -> pure u
    Left t -> readerError $ Text.unpack t

getLogsInfo :: ParserInfo GetLogsArgs
getLogsInfo = info getLogsParser
  ( fullDesc
 <> header "plutus-certification-client run get-logs — Get the logs for a run"
  )

data RunCommand
  = Create !FlakeRefV1
  | Get !RunIDV1
  | Abort !RunIDV1
  | GetLogs !GetLogsArgs

runCommandParser :: Parser RunCommand
runCommandParser = hsubparser
  ( command "create" (Create <$> createRunInfo)
 <> command "get" (Get <$> getRunInfo)
 <> command "abort" (Abort <$> abortRunInfo)
 <> command "get-logs" (GetLogs <$> getLogsInfo)
  )

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

versionCommandInfo :: ParserInfo ()
versionCommandInfo = info (pure ())
  ( fullDesc
 <> header "plutus-certification-client version — Get the version of the server"
  )

data Command
  = CmdRun !RunCommand
  | CmdVersion

commandParser :: Parser Command
commandParser = hsubparser
  ( command "run" (CmdRun <$> runCommandInfo)
 <> command "version" (const CmdVersion <$> versionCommandInfo)
  )

data Args = Args
  { certificationURL :: !BaseUrl
  , certificationUser :: !(Maybe ByteString)
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

main :: IO ()
main = do
  args <- execParser argsInfo
  auth <- case args.certificationUser of
    Nothing -> pure Nothing
    Just u -> runInputT defaultSettings (getPassword Nothing ("password for " ++ (BS.unpack u) ++ ":")) >>= \case
      Just s -> pure . Just . BasicAuthData u $ BS.pack s
      Nothing -> fail "no password provided"
  manager <- newTlsManagerWith $ tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  let apiClient = client $ Proxy @API
      cEnv = mkClientEnv manager args.certificationURL
      cEnv' = case auth of
        Nothing -> cEnv
        Just ba -> cEnv
          { makeClientRequest = \u -> defaultMakeClientRequest u . basicAuthReq ba
          }
      handle :: (ToJSON a) => ClientM a -> IO ()
      handle c = runClientM c cEnv' >>= either throwIO (hPutStrLn stdout . encode)
  case args.cmd of
    CmdVersion -> handle $ apiClient.version
    CmdRun (Create ref) -> handle $ apiClient.createRun ref
    CmdRun (Get ref) -> handle $ apiClient.getRun ref
    CmdRun (Abort ref) -> handle $ (const True <$> apiClient.abortRun ref)
    --TODO: investigate why ZonedTime doesn't serialize properly
    CmdRun (GetLogs (GetLogsArgs ref zt act)) -> handle $ apiClient.getLogs ref zt act
