{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Plutus.Certification.Client where

import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Network.HTTP.Client.TLS
import Servant
import Servant.Client

-- | Action to schedule a crash of an application
newtype ScheduleCrash m = ScheduleCrash { run :: m () }

hmap :: (forall a . m a -> n a) -> ScheduleCrash m -> ScheduleCrash n
hmap f = ScheduleCrash . f . (.run)

-- | Capabilities to make requests against a servant API within a servant server
data ClientCaps c m = ClientCaps
  { -- | Make a request
    runClient :: !(forall a . c a -> m (Either ClientError a))
  , -- | Schedule a crash of the application
    scheduleCrash :: !(ScheduleCrash m)
  , -- | Respond to the current request with an HTTP error
    raiseServerError :: !(forall a . ServerError -> m a)
  }

-- | Run a client request with failures properly managed
--
-- Any errors that may indicate problems on our end will
-- result in a crash, Erlang-style.
runClientOrDie :: Monad m => ClientCaps c m -> c a -> m a
runClientOrDie (ClientCaps {..}) req = runClient req >>= \case
  Left (ConnectionError _) ->
    scheduleCrash.run >> raiseServerError err503
  Left _ -> raiseServerError err502
  Right a -> pure a

-- | Acquire 'ClientCaps' 'ClientM' for some 'MonadIO'
--
-- The resulting client will be TLS-aware
clientCapsIO :: (MonadIO m, MonadError ServerError m)
             => -- | The URL of the upstream server
                BaseUrl
             -> ScheduleCrash m
             -> IO (ClientCaps ClientM m)
clientCapsIO url scheduleCrash = do
  manager <- newTlsManager
  pure $ ClientCaps
    { runClient = liftIO . (flip runClientM $ mkClientEnv manager url)
    , scheduleCrash = scheduleCrash
    , raiseServerError = throwError
    }
