{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Plutus.Certification.Cicero
  ( KnownActionType(..)
  , ActionType(..)
  , CiceroCaps(..)
  , ciceroServerCaps
  ) where

import Conduit
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.KeyMap as KM
import Data.Aeson.QQ
import Data.Functor
import Data.Proxy
import Data.Maybe
import Data.Monoid
import Network.URI
import Servant.Client
import Servant.Client.Core.HasClient
import Observe.Event
import Observe.Event.Render.JSON
import Observe.Event.Servant.Client
import Control.Monad.Catch

import IOHK.Cicero.API qualified as Cicero
import IOHK.Cicero.API.Fact qualified as Cicero.Fact
import IOHK.Cicero.API.Run qualified as Cicero.Run
import IOHK.Cicero.API.Action qualified as Cicero.Action
import IOHK.Cicero.API.Invocation qualified as Cicero.Invocation

import Plutus.Certification.API
import Plutus.Certification.Cache
import Plutus.Certification.Client
import Plutus.Certification.Server

ciceroClient :: forall m . HasClient m Cicero.API => Client m Cicero.API
ciceroClient = cicero `clientIn` m
  where
    cicero = Proxy @Cicero.API

    m = Proxy @m

-- | Types of Cicero actions we know and care about
data KnownActionType
  = -- | @plutus-certification/generate-flake@
    Generate
  | -- | @plutus-certification/build-flake@
    Build
  | -- | @plutus-certification/run-certify@
    Certify

-- | Types of Cicero actions
data ActionType
  = Known !KnownActionType
  | Unknown

-- | Capabilities to implement 'ServerCaps' with Cicero as the job engine
data CiceroCaps c m r = CiceroCaps
  { -- | Client to talk to Cicero
    clientCaps :: !(ClientCaps c m r)
  , -- | Cache of actions we've identified
    actionCache :: !(Cache Cicero.Action.ActionID ActionType m)
  }

data JobEventSelector f where
  ClientErrored :: JobEventSelector ClientError
  -- TODO Domain-specific logging

-- | Implement 'ServerCaps' with Cicero as the job engine
--
-- Jobs are submitted as new @plutus-certification/generate-flake@ facts.
ciceroServerCaps :: forall c m r . (MonadMask m, HasClient c Cicero.API) => CiceroCaps c m r -> ServerCaps m r
ciceroServerCaps CiceroCaps {..} = ServerCaps {..}
  where
    mkClientErrorEv eb parent = do
      ev <- newEvent eb ClientErrored
      addParent ev parent
      pure ev

    submitJob eb parent ref = RunID . (.id.uuid) <$> runClientOrDie clientCaps (mkClientErrorEv eb parent) (setParent clientCaps parent req)
      where
        uri = ref.uri -- aesonQQ's parser doesn't support RecordDot yet
        req = ciceroClient.fact.create $ Cicero.Fact.CreateFact
          { fact = [aesonQQ| { "plutus-certification/generate-flake": { "ref": #{uriToString id uri ""} } } |]
          , artifact = Nothing
          }

    renderJobSel :: RenderSelectorJSON JobEventSelector
    renderJobSel ClientErrored = ("client-error", clientErrorJSON)

    getRuns eb parent rid = go 0
      where
        rid' = Cicero.Fact.FactID $ rid.uuid
        limit = 10
        go offset = do
          runs <- lift . runClientOrDie clientCaps (mkClientErrorEv eb parent) . setParent clientCaps parent $ ciceroClient.run.getAll True [rid'] (Just offset) (Just limit)
          count <- yieldMany runs .| execStateC 0 (status eb parent)
          when (count == limit) $ go (offset + limit)

    status eb parent = await >>= \case
      Nothing -> pure ()
      Just r ->  do
        modify (+ 1)
        inv <- lift . lift $
          runClientOrDie clientCaps (mkClientErrorEv eb parent) . setParent clientCaps parent $ ciceroClient.invocation.get r.invocationId
        ty <- lift . lift $ actionCache.lookup inv.actionId >>= \case
          Just ty -> pure ty
          Nothing -> do
            act <- runClientOrDie clientCaps (mkClientErrorEv eb parent) . setParent clientCaps parent $ ciceroClient.action.get inv.actionId
            let ty = getActionType act
            actionCache.register inv.actionId ty
            pure ty
        case ty of
          Unknown -> pure ()
          Known s -> yieldM . lift $ case s of
            Generate ->
              getOutput eb parent r "plutus-certification/generate-flake" <&> \case
                Nothing -> Preparing Running
                Just (Left _) -> Preparing Failed
                Just (Right _) -> Building Running
            Build ->
              getOutput eb parent r "plutus-certification/build-flake" <&> \case
                Nothing -> Building Running
                Just (Left _) -> Building Failed
                Just (Right _) -> Certifying Running
            Certify ->
              getOutput eb parent r "plutus-certification/run-certify" <&> \case
                Nothing -> Certifying Running
                Just (Left _) -> Certifying Failed
                Just (Right v) -> Finished v
        status eb parent

    getOutput eb parent r name = if isJust r.finishedAt
      then do
        facts <- runClientOrDie clientCaps (mkClientErrorEv eb parent) . setParent clientCaps parent $ ciceroClient.fact.getAll r.nomadJobId
        let getOutput' (Object o)
              | Just (Object out) <- KM.lookup name o
              , Just success <- KM.lookup "success"  out = Just (Right success)
              | Just (Object out) <- KM.lookup name o
              , Just failure <- KM.lookup "failure"  out = Just (Left failure)
              | otherwise = Nothing
            getOutput' _ = Nothing
        pure . getFirst . foldMap (First . getOutput' . (.value)) $ facts
      else pure Nothing

    getActionType :: Cicero.Action.ActionV2 -> ActionType
    getActionType act
      | act.name == "plutus-certification/generate-flake" = Known Generate
      | act.name == "plutus-certification/build-flake" = Known Build
      | act.name == "plutus-certification/run-certify" = Known Certify
      | otherwise = Unknown
