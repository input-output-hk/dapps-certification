{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Exception hiding (Handler)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 as BS hiding (hPutStrLn,foldl')
import Data.Bits
import Data.Function
import Data.Singletons
import Data.Version
import Network.Wai.Handler.Warp
import Data.Foldable
import Network.Wai.Middleware.Cors
import Options.Applicative as Opts
import Servant
import Servant.Client.Core.BaseUrl
import System.Exit
import Observe.Event
import Servant.Server.Experimental.Auth
import Observe.Event.BackendModification
import Observe.Event.Crash
import Observe.Event.Render.JSON
import Observe.Event.Render.IO.JSON
import Observe.Event.Wai hiding (OnException)
import Observe.Event.Servant.Client
import System.IO
import Control.Concurrent.MVar
import Control.Monad
import Control.Concurrent.Async
import Network.Wai
import Plutus.Certification.API
import Plutus.Certification.Cache hiding (lookup)
import Plutus.Certification.Cicero
import Plutus.Certification.Client
import Plutus.Certification.Server
import Plutus.Certification.Local
import Paths_plutus_certification qualified as Package

data Backend
  = Local
  | Cicero !BaseUrl

data Args = Args
  { port :: !Port
  , host :: !HostPreference
  , backend :: !Backend
  }

baseUrlReader :: ReadM BaseUrl
baseUrlReader = do
  urlStr <- str
  case parseBaseUrl urlStr of
    Left e -> case fromException e of
      Just (InvalidBaseUrlException s) -> readerError $ "invalid URL '" ++ urlStr ++ "': " ++ s
      Nothing -> readerError $ "exception parsing '" ++ urlStr ++ "' as a URL: " ++ displayException e
    Right b -> pure b

ciceroParser :: Parser Backend
ciceroParser = Cicero
  <$> option baseUrlReader
      ( long "cicero-url"
     <> metavar "CICERO_URL"
     <> help "URL of the cicero server"
     <> showDefaultWith showBaseUrl
     <> (Opts.value $ BaseUrl Http "localhost" 8080 "")
      )

localParser :: Parser Backend
localParser = flag' Local
  ( long "local"
 <> help "Run with the local in-process \"job scheduler\""
  )
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
  <*> (localParser <|> ciceroParser)

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
  InjectServerSel :: forall f . !(ServerEventSelector f) -> RootEventSelector f
  InjectCrashing :: forall f . !(Crashing f) -> RootEventSelector f
  InjectRunRequest :: forall f . !(RunRequest f) -> RootEventSelector f
  InjectServeRequest :: forall f . !(ServeRequest f) -> RootEventSelector f
  InjectRunClient :: forall f . !(RunClientSelector f) -> RootEventSelector f
  InjectLocal :: forall f . !(LocalSelector f) -> RootEventSelector f

renderRoot :: RenderSelectorJSON RootEventSelector
renderRoot Initializing =
  ( "initializing"
  , \case
      ArgsField args -> ("args", object [ "port" .= args.port
                                        , "host" .= show args.host
                                        , "backend" .= case args.backend of
                                            Cicero u -> object [ "cicero-url" .= u ]
                                            Local -> String "local"
                                        ])
      VersionField v -> ("version", toJSON $ versionBranch v)
  )
renderRoot OnException =
  ( "handling-exception"
  , renderOnExceptionField renderJSONException
  )
renderRoot (InjectServerSel serverSel) =
  renderServerEventSelector serverSel
renderRoot (InjectCrashing s) = renderCrashing s
renderRoot (InjectRunRequest s) = runRequestJSON s
renderRoot (InjectServeRequest s) = renderServeRequest s
renderRoot (InjectRunClient s) = renderRunClientSelector s
renderRoot (InjectLocal s) = renderLocalSelector s

--TODO: remove after proper DB is implemented
dummyHash :: ByteString -> Int
dummyHash = foldl' (\h c -> 33*h `xor` fromEnum c) 5381 . unpack

authHandler :: AuthHandler Request UserId
authHandler = mkAuthHandler handler
  where
  maybeToEither e = maybe (Left e) Right
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req = either throw401 (pure . UserId . dummyHash) $ do
    --TODO: this is a subject of change after MVP 2.0
    maybeToEither "Missing Authorization header" $ lookup "Authorization" $ requestHeaders req

genAuthServerContext :: Context (AuthHandler Request UserId ': '[])
genAuthServerContext = authHandler :. EmptyContext

main :: IO ()
main = do
  args <- execParser opts

  eb <- jsonHandleBackend stderr renderJSONException renderRoot
  withEvent eb Initializing \initEv -> do
    addField initEv $ VersionField Package.version
    addField initEv $ ArgsField args

    crashVar <- newEmptyMVar
    closeSocketVar <- newEmptyMVar
    let doCrash = void $ tryPutMVar crashVar ()
        waitForCrash = do
          (closeSocket, ()) <- concurrently (takeMVar closeSocketVar) (takeMVar crashVar)
          closeSocket
    withAsync waitForCrash \_ -> withScheduleCrash (narrowEventBackend InjectCrashing eb) doCrash \scheduleCrash -> do
      caps <- case args.backend of
        Cicero ciceroUrl -> do
          actionCache <- newCacheMapIO

          clientCaps <- clientCapsIO
            ciceroUrl
            (hoistScheduleCrash liftIO scheduleCrash)
            ( hoistEventBackend liftIO
            . narrowEventBackend InjectRunRequest
            $ eb
            )
          pure . ciceroServerCaps ( hoistEventBackend liftIO
                                  . narrowEventBackend InjectRunClient
                                  $ eb
                                  ) $ CiceroCaps {..}
        Local -> hoistServerCaps liftIO <$> localServerCaps ( narrowEventBackend InjectLocal
                                                            $ eb
                                                            )
      let settings = defaultSettings
                   & setPort args.port
                   & setHost args.host
                   & setOnException (\r e -> when (defaultShouldDisplayException e) $ withEvent eb OnException \ev -> do
                                        addField ev $ OnExceptionField r e
                                        schedule scheduleCrash (setAncestor $ reference ev))
                   & setInstallShutdownHandler (putMVar closeSocketVar)
                   & setBeforeMainLoop (finalize initEv)
          corsPolicy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type"] }

      runSettings settings . application (narrowEventBackend InjectServeRequest eb) $
        cors (const $ Just corsPolicy) .
        serveWithContext (Proxy @API) genAuthServerContext .
        (\r -> server caps (hoistEventBackend liftIO  $ narrowEventBackend InjectServerSel $ modifyEventBackend (setAncestor r) eb))
  exitFailure
