{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Plutus.Certification.Client where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Observe.Event
import Observe.Event.Wai
import Observe.Event.Servant.Client
import Servant
import Servant.Client

-- | Capabilities to make requests against a servant API within a servant server
data ClientCaps c m r = ClientCaps
  { -- | Make a request
    runClient :: !(forall a . c a -> m (Either ClientError a))
  , -- | Schedule a crash of the application
    scheduleShutdown :: !(ScheduleShutdown m r)
  , -- | Respond to the current request with an HTTP error
    raiseServerError :: !(forall a . ServerError -> m a)
  , -- | Make a referred-to event the parent of all request events
    setParent :: !(forall a . r -> c a -> c a)
  }

-- | Run a client request with failures properly managed
--
-- Any errors that may indicate problems on our end will
-- result in a crash, Erlang-style.
runClientOrDie :: (MonadMask m) => ClientCaps c m r -> m (Event m r ClientError) -> c a -> m a
runClientOrDie (ClientCaps {..}) mkErrE req = runClient req >>= \case
  Left e -> withEvent mkErrE \ev -> do
    addField ev e
    case e of
      ConnectionError _ -> scheduleShutdown (Just (ref ev)) >> raiseServerError err503
      _ -> raiseServerError err502
  Right a -> pure a

-- | Acquire 'ClientCaps' 'EventfulClientM' for some 'MonadIO'
--
-- The resulting client will be TLS-aware
clientCapsIO :: (MonadIO m, MonadError ServerError m)
             => -- | The URL of the upstream server
                BaseUrl
             -> ScheduleShutdown m r
             -> ClientM (Event ClientM r RunRequestField)
             -> IO (ClientCaps (EventfulClientM r) m r)
clientCapsIO url scheduleShutdown doE = do
  manager <- newTlsManagerWith $ tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  pure $ ClientCaps
    { runClient = liftIO . flip (runEventfulClientM doE) (mkClientEnv manager url)
    , raiseServerError = throwError
    , setParent = withParent
    , ..
    }
