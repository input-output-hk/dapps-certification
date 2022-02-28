{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Servant
import Network.Wai.Handler.Warp
import Options.Applicative
import Data.Function
import Servant.Client
import Servant.Client.Core.BaseUrl
import Control.Monad.IO.Class
import Data.Aeson.QQ
import Network.URI
import Control.Exception
import Data.ByteString.Lazy.Char8
import Data.Streaming.Network (bindPortTCP)
import Network.Socket
import Control.Concurrent.MVar
import Control.Concurrent.Async
import System.Exit
import Data.Functor
import Network.HTTP.Client.TLS

import IOHK.Cicero.API qualified as Cicero
import IOHK.Cicero.API.Fact qualified as Cicero.Fact

import Plutus.Certification.API
import Paths_plutus_certification qualified as Package

data Env = Env
  { clientEnv :: !ClientEnv
  , crashSem :: !(MVar ())
  }

server :: Env -> Server API
server e = NamedAPI
    { version = pure $ VersionV1 Package.version
    , versionHead = pure NoContent
    , createRun = \ref -> do
        let uriStr = uriToString id ref.uri "" -- aesonQQ's parser doesn't support OverloadedRecordDot yet
            req = ciceroClient.fact.create $ Cicero.Fact.CreateFact
              { fact = [aesonQQ| { "plutus-certification/generate-flake": { "ref": #{uriStr} } } |]
              , artifact = Nothing
              }
        (liftIO $ runClientM req e.clientEnv) >>= \case
          Left (ConnectionError err) -> do
            liftIO $ tryPutMVar e.crashSem ()
            throwError $ err503
              { errBody = append "Couldn't connect to Cicero: " (pack $ displayException err)
              }
          Left e ->
            throwError $ err502
              { errBody = append "Unexpecte response from Cicero: " (pack $ displayException e)
              }
          Right f -> pure $ RunID f.id
    }
  where
    ciceroClient = client $ Proxy @Cicero.API

app :: Env -> Application
app = serve (Proxy @API) . server

data Args = Args
  { port :: !Port
  , host :: !HostPreference
  , ciceroURL :: !BaseUrl
  }

baseUrlReader :: ReadM BaseUrl
baseUrlReader = do
  urlStr <- str
  case parseBaseUrl urlStr of
    Left e -> case fromException e of
      Just (InvalidBaseUrlException s) -> readerError $ "invalid URL '" ++ urlStr ++ "': " ++ s
      Nothing -> readerError $ "exception parsing '" ++ urlStr ++ "' as a URL: " ++ displayException e
    Right b -> pure b

args :: Parser Args
args =  Args
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "server port number"
     <> showDefault
     <> value 9671
      )
  <*> option auto
      ( long "bind"
     <> short 'b'
     <> metavar "HOST"
     <> help "server bind address"
     <> showDefault
     <> value "*6"
      )
  <*> option baseUrlReader
        ( long "cicero-url"
       <> metavar "CICERO_URL"
       <> help "URL of the cicero server"
       <> showDefaultWith showBaseUrl
       <> (value $ BaseUrl Http "localhost" 8080 "")
        )

opts :: ParserInfo Args
opts = info (args <**> helper)
  ( fullDesc
 <> progDesc "Run the plutus-certification server"
 <> header "plutus-certification — Certification as a service for Plutus applications"
  )

main :: IO ()
main = do
  args <- execParser opts

  sock <- bindPortTCP args.port args.host
  crashSem <- newEmptyMVar

  manager <- newTlsManager

  let settings = defaultSettings
               & setPort args.port
               & setHost args.host
               & setOnException (\_ _ -> void $ tryPutMVar crashSem ())

  withAsync (takeMVar crashSem >> close' sock) \_ ->
    runSettingsSocket settings sock . app $ Env
      { clientEnv = mkClientEnv manager args.ciceroURL
      , crashSem = crashSem
      }
  exitFailure
