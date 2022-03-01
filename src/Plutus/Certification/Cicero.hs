{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternGuards #-}
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
import Data.UUID
import Data.Maybe
import Data.Monoid
import Network.URI
import Numeric.Natural
import Servant.Client
import Servant.Client.Core.HasClient


import IOHK.Cicero.API qualified as Cicero
import IOHK.Cicero.API.Fact qualified as Cicero.Fact
import IOHK.Cicero.API.Run qualified as Cicero.Run
import IOHK.Cicero.API.Action qualified as Cicero.Action

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
data CiceroCaps c m = CiceroCaps
  { -- | Client to talk to Cicero
    clientCaps :: !(ClientCaps c m)
  , -- | Cache of actions we've identified
    actionCache :: !(Cache UUID ActionType m)
  }

-- | Implement 'ServerCaps' with Cicero as the job engine
--
-- Jobs are submitted as new @plutus-certification/generate-flake@ facts.
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
        ty <- lift . lift $ caps.actionCache.lookup r.actionId >>= \case
          Just ty -> pure ty
          Nothing -> do
            act <- runClientOrDie caps.clientCaps $ ciceroClient.action.get r.actionId
            let ty = getActionType act
            caps.actionCache.register r.actionId ty
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
