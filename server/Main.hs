{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Exception hiding (Handler)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Function
import Data.Singletons
import Data.Version
import Data.Void
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Options.Applicative as Opts
import Servant
import Servant.Client.Core.BaseUrl
import System.Exit
import Observe.Event
import Observe.Event.Render.JSON
import Observe.Event.Render.IO.JSON
import Observe.Event.Wai
import Observe.Event.Servant.Client
import System.IO

import Plutus.Certification.API
import Plutus.Certification.Cache
import Plutus.Certification.Cicero
import Plutus.Certification.Client
import Plutus.Certification.Server
import Paths_plutus_certification qualified as Package

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

argsParser :: Parser Args
argsParser =  Args
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "server port number"
     <> showDefault
     <> Opts.value 9671
      )
  <*> option str
      ( long "bind"
     <> short 'b'
     <> metavar "HOST"
     <> help "server bind address"
     <> showDefault
     <> Opts.value "*"
      )
  <*> option baseUrlReader
      ( long "cicero-url"
     <> metavar "CICERO_URL"
     <> help "URL of the cicero server"
     <> showDefaultWith showBaseUrl
     <> (Opts.value $ BaseUrl Http "localhost" 8080 "")
      )

opts :: ParserInfo Args
opts = info (argsParser <**> helper)
  ( fullDesc
 <> progDesc "Run the plutus-certification server"
 <> header "plutus-certification — Certification as a service for Plutus applications"
  )

data InitializingField
  = ArgsField Args
  | VersionField Version

data RootEventSelector f where
  Initializing :: RootEventSelector InitializingField
  OnException :: RootEventSelector OnExceptionField
  Request :: RootEventSelector RequestField
  Shutdown :: RootEventSelector Void
  Client :: RootEventSelector RunRequestField
  InjectServerSel :: forall f . !(ServerEventSelector f) -> RootEventSelector f

renderRoot :: RenderSelectorJSON RootEventSelector
renderRoot Initializing =
  ( "initializing"
  , \case
      ArgsField args -> ("args", object [ "port" .= args.port
                                        , "host" .= show args.host
                                        , "cicero-url" .= args.ciceroURL
                                        ])
      VersionField v -> ("version", toJSON $ versionBranch v)
  )
renderRoot OnException =
  ( "handling-exception"
  , renderOnExceptionField renderJSONException
  )
renderRoot Request =
  ( "handling-request"
  , renderRequestField
  )
renderRoot Shutdown =
  ( "shutting-down"
  , absurd
  )
renderRoot Client =
  ( "running-client-request"
  , runRequestFieldJSON
  )
renderRoot (InjectServerSel serverSel) =
  renderServerEventSelector serverSel

main :: IO ()
main = do
  args <- execParser opts

  eb <- jsonHandleBackend stderr renderJSONException renderRoot
  withEvent (newEvent eb Initializing) \initEv -> do
    addField initEv $ VersionField Package.version
    addField initEv $ ArgsField args

    actionCache <- newCacheMapIO

    withScheduleShutdownHandler (newEvent eb Shutdown) \scheduleShutdown shutdownHandler -> do
      clientCaps <- clientCapsIO
        args.ciceroURL
        (hoistScheduleShutdown liftIO scheduleShutdown)
        (newEvent (hoistEventBackend liftIO eb) Client)

      let settings = defaultSettings
                   & setPort args.port
                   & setHost args.host
                   & setOnException (eventfulOnException do
                                        ev <- newEvent eb OnException
                                        scheduleShutdown (Just (ref ev))
                                        pure ev)
                   & setInstallShutdownHandler shutdownHandler
                   & setBeforeMainLoop (finalize initEv)
          caps = ciceroServerCaps $ CiceroCaps {..}

      runSettings settings . eventfulApplication (newEvent eb Request) $
        simpleCors . serve (Proxy @API) . server caps (hoistEventBackend liftIO (narrowEventBackend InjectServerSel eb))
  exitFailure
