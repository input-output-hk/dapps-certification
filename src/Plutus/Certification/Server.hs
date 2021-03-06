{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Plutus.Certification.Server where

import Conduit
import Control.Monad.Catch
import Data.Aeson
import Data.Void
import Servant
import Control.Monad.State.Strict
import Observe.Event
import Observe.Event.Render.JSON
import Network.URI

import Plutus.Certification.API
import Paths_plutus_certification qualified as Package

-- | Capabilities needed to run a server for 'API'
data ServerCaps m r = forall jobSel . ServerCaps
  { renderJobSel :: !(RenderSelectorJSON jobSel)
  , -- | Submit a new certification job
    submitJob :: !(EventBackend m r jobSel -> r -> FlakeRefV1 -> m RunIDV1)
  , -- | Get the status of all runs associated with a job
    getRuns :: !(EventBackend m r jobSel -> r -> RunIDV1 -> ConduitT () RunStatusV1 m ())
  }

data CreateRunField
  = CreateRunRef !FlakeRefV1
  | CreateRunID !RunIDV1

data ServerEventSelector f where
  Version :: ServerEventSelector Void
  CreateRun :: ServerEventSelector CreateRunField
  GetRun :: ServerEventSelector Void
  InjectJobSel :: forall jobSel f . !(RenderSelectorJSON jobSel) -> !(jobSel f) -> ServerEventSelector f

renderServerEventSelector :: RenderSelectorJSON ServerEventSelector
renderServerEventSelector Version = ("version", absurd)
renderServerEventSelector CreateRun = ("create-run", \case
                                            CreateRunRef fr -> ("flake-reference", toJSON $ uriToString id fr.uri "")
                                            CreateRunID rid -> ("run-id", toJSON rid)
                                        )
renderServerEventSelector GetRun = ("get-run", absurd)
renderServerEventSelector (InjectJobSel renderJobSel s) = renderJobSel s

-- | An implementation of 'API'
server :: (MonadMask m) => ServerCaps m r -> EventBackend m r ServerEventSelector -> r -> ServerT API m
server ServerCaps {..} eb parent = NamedAPI
  { version = withSubEvent eb parent Version . const . pure $ VersionV1 Package.version
  , versionHead = withSubEvent eb parent Version . const $ pure NoContent
  , createRun = \fref -> withSubEvent eb parent CreateRun \ev -> do
      addField ev $ CreateRunRef fref
      res <- submitJob jobEb (ref ev) fref
      addField ev $ CreateRunID res
      pure res
  , getRun = \rid -> withSubEvent eb parent GetRun \ev ->
     runConduit
        $ getRuns jobEb (ref ev) rid
       .| execStateC Queued consumeRuns
  }
  where
    jobEb = narrowEventBackend (InjectJobSel renderJobSel) eb
    consumeRuns = await >>= \case
      Nothing -> pure ()
      Just s@(Finished _) -> put s
      Just s -> do
        modify $ max s
        consumeRuns
