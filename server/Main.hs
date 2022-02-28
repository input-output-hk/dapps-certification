{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}
module Main where

import Servant
import Servant.Server.Internal
import Network.Wai.Handler.Warp
import Options.Applicative
import Data.Function
import Servant.Client
import Servant.Client.Core.BaseUrl
import Servant.Client.Core.HasClient
import Control.Monad.IO.Class
import Data.Aeson.QQ
import Network.URI
import Control.Exception hiding (Handler)
import Data.ByteString.Lazy.Char8
import Data.Streaming.Network (bindPortTCP)
import Network.Socket
import Control.Concurrent.MVar
import Control.Concurrent.Async
import System.Exit
import Data.Functor
import Data.UUID
import Network.HTTP.Client.TLS

import IOHK.Cicero.API qualified as Cicero
import IOHK.Cicero.API.Fact qualified as Cicero.Fact

import Plutus.Certification.API
import Paths_plutus_certification qualified as Package

type RunClient c m = forall a . c a -> m (Either ClientError a)

data ClientCaps c m = ClientCaps
  { runClient :: RunClient c m
  , scheduleCrash :: m ()
  , raiseServerError :: forall a . ServerError -> m a
  }

runClientOrDie :: Monad m => ClientCaps c m -> c a -> m a
runClientOrDie (ClientCaps {..}) req = runClient req >>= \case
  Left (ConnectionError _) ->
    scheduleCrash >> raiseServerError err503
  Left _ -> raiseServerError err502
  Right a -> pure a

ciceroClient :: forall m . HasClient m Cicero.API => Client m Cicero.API
ciceroClient = cicero `clientIn` m
  where
    cicero = Proxy @Cicero.API

    m = Proxy @m

ciceroSubmitJob :: (Monad m, HasClient c Cicero.API) => ClientCaps c m -> URI -> m UUID
ciceroSubmitJob caps uri  = (.id) <$> runClientOrDie caps req
  where
    req = ciceroClient.fact.create $ Cicero.Fact.CreateFact
      { fact = [aesonQQ| { "plutus-certification/generate-flake": { "ref": #{uriToString id uri ""} } } |]
      , artifact = Nothing
      }

data ServerCaps m = ServerCaps
  { submitJob :: !(URI -> m UUID)
  }

server :: Applicative m => ServerCaps m -> NamedAPI (AsServerT m)
server caps = NamedAPI
  { version = pure $ VersionV1 Package.version
  , versionHead = pure NoContent
  , createRun = \ref -> RunID <$> caps.submitJob ref.uri
  }

app :: ServerCaps Handler -> Application
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
      cEnv = mkClientEnv manager args.ciceroURL
      clientCaps = ClientCaps
        { runClient = liftIO . flip runClientM cEnv
        , scheduleCrash = void . liftIO $ tryPutMVar crashSem ()
        , raiseServerError = throwError
        }

  withAsync (takeMVar crashSem >> close' sock) \_ ->
    runSettingsSocket settings sock . app $ ServerCaps
      { submitJob = ciceroSubmitJob clientCaps
      }
  exitFailure
