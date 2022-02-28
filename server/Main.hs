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
{-# LANGUAGE PatternGuards #-}
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
import Conduit
import Control.Monad.State.Strict
import Numeric.Natural
import Data.Aeson
import Data.Monoid
import Data.Maybe
import Data.Aeson.KeyMap as KM
import Data.Map qualified as Map
import Data.IORef

import IOHK.Cicero.API qualified as Cicero
import IOHK.Cicero.API.Fact qualified as Cicero.Fact
import IOHK.Cicero.API.Run qualified as Cicero.Run
import IOHK.Cicero.API.Action qualified as Cicero.Action

import Plutus.Certification.API
import Paths_plutus_certification qualified as Package

data ServerCaps m = ServerCaps
  { submitJob :: !(URI -> m UUID)
  , getRuns :: !(UUID -> ConduitT () RunStatusV1 m ())
  }

server :: Monad m => ServerCaps m -> NamedAPI (AsServerT m)
server caps = NamedAPI
  { version = pure $ VersionV1 Package.version
  , versionHead = pure NoContent
  , createRun = \ref -> RunID <$> caps.submitJob ref.uri
  , getRun = \rid ->
     runConduit
        $ caps.getRuns rid.uuid
       .| execStateC Queued consumeRuns
  }
  where
    consumeRuns = await >>= \case
      Nothing -> pure ()
      Just s@(Finished _) -> lift $ put s
      Just s ->
        (modify $ max s) >> consumeRuns

app :: ServerCaps Handler -> Application
app = serve (Proxy @API) . server

data ClientCaps c m = ClientCaps
  { runClient :: !(forall a . c a -> m (Either ClientError a))
  , scheduleCrash :: !(m ())
  , raiseServerError :: !(forall a . ServerError -> m a)
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

data KnownActionType
  = Generate
  | Build
  | Certify

data ActionType
  = Known !KnownActionType
  | Unknown

data CiceroCaps c m = CiceroCaps
  { clientCaps :: !(ClientCaps c m)
  , lookupActionType :: !(UUID -> m (Maybe ActionType))
  , registerActionType :: !(UUID -> ActionType -> m ())
  }

ciceroServerCaps :: forall c m . (Monad m, HasClient c Cicero.API) => CiceroCaps c m -> ServerCaps m
ciceroServerCaps caps = ServerCaps {..}
  where
    submitJob :: URI -> m UUID
    submitJob uri = (.id) <$> runClientOrDie caps.clientCaps req
      where
        req = ciceroClient.fact.create $ Cicero.Fact.CreateFact
          { fact = [aesonQQ| { "plutus-certification/generate-flake": { "ref": #{uriToString id uri ""} } } |]
          , artifact = Nothing
          }
    getRuns :: UUID -> ConduitT () RunStatusV1 m ()
    getRuns rid = go 0
      where
        limit = 10
        go offset = do
          runs <- lift . runClientOrDie caps.clientCaps $ ciceroClient.run.getAll True [rid] (Just offset) (Just limit)
          count <- yieldMany runs .| execStateC 0 status
          when (count == limit) $ go (offset + limit)

    status :: ConduitT Cicero.Run.RunV1 RunStatusV1 (StateT Natural m) ()
    status = await >>= \case
      Nothing -> pure ()
      Just r ->  do
        modify (+ 1)
        ty <- lift . lift $ caps.lookupActionType r.actionId >>= \case
          Just ty -> pure ty
          Nothing -> do
            act <- runClientOrDie caps.clientCaps $ ciceroClient.action.get r.actionId
            let ty = getActionType act
            caps.registerActionType r.actionId ty
            pure ty
        case ty of
          Unknown -> pure ()
          Known s -> yieldM . lift $ case s of
            Generate ->
              getOutput r "plutus-certification/generate-flake" <&> \case
                Nothing -> Preparing Running
                Just (Left _) -> Preparing Failed
                Just (Right _) -> Building Running
            Build ->
              getOutput r "plutus-certification/build-flake" <&> \case
                Nothing -> Building Running
                Just (Left _) -> Building Failed
                Just (Right _) -> Certifying Running
            Certify ->
              getOutput r "plutus-certification/run-certify" <&> \case
                Nothing -> Certifying Running
                Just (Left _) -> Certifying Failed
                Just (Right v) -> Finished v
        status

    getOutput :: Cicero.Run.RunV1 -> Key -> m (Maybe (Either Value Value))
    getOutput r name = if isJust r.finishedAt
      then do
        facts <- runClientOrDie caps.clientCaps $ ciceroClient.fact.getAll r.nomadJobId
        let getOutput (Object o)
              | Just (Object out) <- KM.lookup name o
              , Just success <- KM.lookup "success"  out = Just (Right success)
              | Just (Object out) <- KM.lookup name o
              , Just failure <- KM.lookup "failure"  out = Just (Left failure)
              | otherwise = Nothing
            getOutput _ = Nothing
        pure . getFirst . foldMap (First . getOutput . (.value)) $ facts
      else pure Nothing

    getActionType :: Cicero.Action.ActionV1 -> ActionType
    getActionType act
      | act.name == "plutus-certification/generate-flake" = Known Generate
      | act.name == "plutus-certification/build-flake" = Known Build
      | act.name == "plutus-certification/run-certify" = Known Certify
      | otherwise = Unknown

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

  actionTypes <- newIORef Map.empty
  let settings = defaultSettings
               & setPort args.port
               & setHost args.host
               & setOnException (\_ _ -> void $ tryPutMVar crashSem ())
      cEnv = mkClientEnv manager args.ciceroURL
      caps = ciceroServerCaps $ CiceroCaps
        { clientCaps = ClientCaps
            { runClient = liftIO . flip runClientM cEnv
            , scheduleCrash = void . liftIO $ tryPutMVar crashSem ()
            , raiseServerError = throwError
            }
        , lookupActionType = \uuid -> liftIO $
            readIORef actionTypes <&> Map.lookup uuid
        , registerActionType = \uuid ty -> liftIO $ atomicModifyIORef' actionTypes \tys ->
            (Map.insert uuid ty tys, ())
        }

  withAsync (takeMVar crashSem >> close' sock) \_ ->
    runSettingsSocket settings sock $ app caps
  exitFailure
