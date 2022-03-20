{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception hiding (Handler)
import Control.Monad.IO.Class
import Data.Function
import Data.Functor
import Network.Wai.Handler.Warp
import Options.Applicative
import Servant
import Servant.Client.Core.BaseUrl
import System.Exit

import Plutus.Certification.API
import Plutus.Certification.Cache
import Plutus.Certification.Cicero
import Plutus.Certification.Client
import Plutus.Certification.Server

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
  <*> option str
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

withShutdownHandler :: (ScheduleCrash IO -> (IO () -> IO ()) -> IO a) -> IO a
withShutdownHandler go = do
  closeSocketChan <- newEmptyMVar
  crashSem <- newEmptyMVar
  let waitForCrash = do
        (closeSocket, ()) <- concurrently (takeMVar closeSocketChan) (takeMVar crashSem)
        closeSocket
  withAsync waitForCrash \_ ->
    go (ScheduleCrash . void $ tryPutMVar crashSem ()) (putMVar closeSocketChan)

main :: IO ()
main = do
  args <- execParser opts

  actionCache <- cacheMapIO

  withShutdownHandler \scheduleCrash shutdownHandler -> do
    clientCaps <- clientCapsIO args.ciceroURL $ hmap liftIO scheduleCrash

    let settings = defaultSettings
                 & setPort args.port
                 & setHost args.host
                 & setOnException (\_ _ -> scheduleCrash.run)
                 & setInstallShutdownHandler shutdownHandler
        caps = ciceroServerCaps $ CiceroCaps {..}

    runSettings settings $ app caps
  exitFailure
