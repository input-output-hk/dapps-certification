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
import Observe.Event.BackendModification
import Observe.Event.Render.JSON
import Network.URI
import IOHK.Certification.Interface

import Plutus.Certification.API
import Paths_plutus_certification qualified as Package

-- | Capabilities needed to run a server for 'API'
data ServerCaps m r = ServerCaps
  { -- | Submit a new certification job
    submitJob :: !(EventBackendModifiers r r -> FlakeRefV1 -> m RunIDV1)
  , -- | Get the status of all runs associated with a job
    getRuns :: !(EventBackendModifiers r r -> RunIDV1 -> ConduitT () RunStatusV1 m ())
  , -- | Delete all runs associated with a job
    abortRuns :: !(EventBackendModifiers r r -> RunIDV1 -> ConduitT () () m ())
  }

hoistServerCaps :: (Monad m) => (forall x . m x -> n x) -> ServerCaps m r -> ServerCaps n r
hoistServerCaps nt (ServerCaps {..}) = ServerCaps
  { submitJob = \mods -> nt . submitJob mods
  , getRuns = \mods -> transPipe nt . getRuns mods
  , abortRuns = \mods -> transPipe nt . abortRuns mods
  }

data CreateRunField
  = CreateRunRef !FlakeRefV1
  | CreateRunID !RunIDV1

data ServerEventSelector f where
  Version :: ServerEventSelector Void
  CreateRun :: ServerEventSelector CreateRunField
  GetRun :: ServerEventSelector Void
  AbortRun :: ServerEventSelector RunIDV1

renderServerEventSelector :: RenderSelectorJSON ServerEventSelector
renderServerEventSelector Version = ("version", absurd)
renderServerEventSelector CreateRun = ("create-run", \case
                                            CreateRunRef fr -> ("flake-reference", toJSON $ uriToString id fr.uri "")
                                            CreateRunID rid -> ("run-id", toJSON rid)
                                        )
renderServerEventSelector GetRun = ("get-run", absurd)
renderServerEventSelector AbortRun = ("abort-run", \rid -> ("run-id",toJSON rid))

-- | An implementation of 'API'
server :: (MonadMask m) => ServerCaps m r -> EventBackend m r ServerEventSelector -> ServerT API m
server ServerCaps {..} eb = NamedAPI
  { version = withEvent eb Version . const . pure $ VersionV1 Package.version
  , versionHead = withEvent eb Version . const $ pure NoContent
  , createRun = \fref -> withEvent eb CreateRun \ev -> do
      addField ev $ CreateRunRef fref
      res <- submitJob (setAncestor $ reference ev) fref
      addField ev $ CreateRunID res
      pure res
  , getRun = \rid -> withEvent eb GetRun \ev ->
     runConduit
        $ getRuns (setAncestor $ reference ev) rid
       .| evalStateC Queued consumeRuns
  , abortRun = \rid -> withEvent eb AbortRun \ev -> do
     const NoContent <$> (runConduit $ abortRuns (setAncestor $ reference ev) rid .| await)
  }
  where
    consumeRuns = await >>= \case
      Nothing -> Incomplete <$> get
      Just (Incomplete s) -> do
        modify \s' -> case (s, s') of
          (_, Queued) -> s
          (Queued, _) -> s'

          (Preparing st, Preparing st') -> case compare st st' of
            LT -> s'
            _ -> s
          (_, Preparing _) -> s
          (Preparing _, _) -> s'

          (Building st, Building st') -> case compare st st' of
            LT -> s'
            _ -> s
          (_, Building _) -> s
          (Building _, _) -> s'

          (Certifying (CertifyingStatus st mp _), Certifying (CertifyingStatus st' mp' _)) -> case compare st st' of
            LT -> s'
            GT -> s
            EQ -> case (mp, mp') of
              (_, Nothing) -> s
              (Nothing, _) -> s'
              (Just p, Just p') -> case compare p.progressIndex p'.progressIndex of
                LT -> s'
                _ -> s
        consumeRuns
      Just s -> pure s
