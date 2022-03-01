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
import Control.Monad.State.Strict

import Plutus.Certification.API
import Paths_plutus_certification qualified as Package

-- | Capabilities needed to run a server for 'API'
data ServerCaps m = ServerCaps
  { -- | Submit a new certification job
    submitJob :: !(FlakeRefV1 -> m RunIDV1)
  , -- | Get the status of all runs associated with a job
    getRuns :: !(RunIDV1 -> ConduitT () RunStatusV1 m ())
  }

-- | An implementation of 'API'
server :: Monad m => ServerCaps m -> ServerT API m
server caps = NamedAPI
  { version = pure $ VersionV1 Package.version
  , versionHead = pure NoContent
  , createRun = caps.submitJob
  , getRun = \rid ->
     runConduit
        $ caps.getRuns rid
       .| execStateC Queued consumeRuns
  }
  where
    consumeRuns = await >>= \case
      Nothing -> pure ()
      Just s@(Finished _) -> lift $ put s
      Just s ->
        (modify $ max s) >> consumeRuns
