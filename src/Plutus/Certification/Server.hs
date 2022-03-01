{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Plutus.Certification.Server where

import Conduit
import Data.UUID
import Network.URI
import Servant
import Servant.Server.Internal
import Control.Monad.State.Strict

import Plutus.Certification.API
import Paths_plutus_certification qualified as Package

-- | Capabilities needed to run a server for 'API'
data ServerCaps m = ServerCaps
  { -- | Submit a new certification job
    submitJob :: !(URI -> m UUID)
  , -- | Get the status of all runs associated with a job
    getRuns :: !(UUID -> ConduitT () RunStatusV1 m ())
  }

-- | An implementation of 'API'
--
-- prop> NamedAPI (AsServerT Handler) ~ Server API
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
