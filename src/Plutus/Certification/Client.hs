{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
module Plutus.Certification.Client where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Observe.Event
import qualified Observe.Event.Servant.Client as EClient
import Observe.Event.Crash
import Observe.Event.BackendModification
import Servant
import Servant.Client

-- | Capabilities to make requests against a servant API within a servant server
data ClientCaps c m r = ClientCaps
  { -- | Make a request
    runClient :: !(forall a . EventBackendModifiers r r -> c a -> m (Either ClientError a))
  , -- | Schedule a crash of the application
    scheduleCrash :: !(ScheduleCrash m r)
  , -- | Respond to the current request with an HTTP error
    raiseServerError :: !(forall a . ServerError -> m a)
  }

-- | Run a client request with failures properly managed
--
-- Any errors that may indicate problems on our end will
-- result in a crash, Erlang-style.
runClientOrDie :: (MonadMask m) => ClientCaps c m r -> EventBackend m r RunClientSelector -> c a -> m a
runClientOrDie (ClientCaps {..}) backend req = withEvent backend RunClient \ev -> let mods = setAncestor $ reference ev in
  runClient mods req >>= \case
  Left e -> do
    addField ev e
    case e of
      ConnectionError _ -> schedule scheduleCrash mods >> raiseServerError err503
      _ -> raiseServerError err502
  Right a -> pure a

-- | Acquire 'ClientCaps' 'EventfulClientM' for some 'MonadIO'
--
-- The resulting client will be TLS-aware
clientCapsIO :: (MonadIO m, MonadError ServerError m)
             => -- | The URL of the upstream server
                BaseUrl
             -> ScheduleCrash m r
             -> EventBackend ClientM r EClient.RunRequest
             -> IO (ClientCaps (EClient.ClientM r) m r)
clientCapsIO url scheduleCrash backend = do
  manager <- newTlsManagerWith $ tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  pure $ ClientCaps
    { runClient = \mods -> liftIO . flip (EClient.runClientM (modifyEventBackend mods backend)) (mkClientEnv manager url)
    , raiseServerError = throwError
    , ..
    }

data RunClientSelector f where
  RunClient :: RunClientSelector ClientError
