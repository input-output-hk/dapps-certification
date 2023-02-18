{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
module Plutus.Certification.Cicero
  ( KnownActionType(..)
  , ActionType(..)
  , CiceroCaps(..)
  , ciceroServerCaps
  , renderRunClientSelector
  ) where

import Conduit
import Control.Monad.State.Strict
import Data.Aeson
import Data.Aeson.KeyMap as KM hiding (foldr)
import Data.Aeson.QQ
import Data.Functor
import Data.Proxy
import Data.Maybe
import Data.Monoid
import Network.URI
import Servant.Client
import Servant.Client.Core.HasClient
import Observe.Event
import Observe.Event.BackendModification
import Observe.Event.Render.JSON
import Observe.Event.Servant.Client
import Control.Monad.Catch
import IOHK.Certification.Interface qualified as I

import IOHK.Cicero.API qualified as Cicero
import IOHK.Cicero.API.Fact qualified as Cicero.Fact
import IOHK.Cicero.API.Run qualified as Cicero.Run
import IOHK.Cicero.API.Action qualified as Cicero.Action
import IOHK.Cicero.API.Invocation qualified as Cicero.Invocation

import Data.List qualified as List

import Plutus.Certification.API
import Plutus.Certification.Cache
import Plutus.Certification.Client
import Plutus.Certification.Server

ciceroClient :: forall m . HasClient m Cicero.API => Client m Cicero.API
ciceroClient = cicero `clientIn` m
  where
    cicero = Proxy @Cicero.API

    m = Proxy @m

-- | Types of Cicero actions
data ActionType
  = Known !KnownActionType
  | Unknown
  deriving stock (Show,Eq,Ord)

data CertifyOutput
  = CertifyFailed
  | Succeeded !I.CertificationResult
  | Intermediate !I.Progress

-- | Capabilities to implement 'ServerCaps' with Cicero as the job engine
data CiceroCaps c m r = CiceroCaps
  { -- | Client to talk to Cicero
    clientCaps :: !(ClientCaps c m r)
  , -- | Cache of actions we've identified
    actionCache :: !(Cache Cicero.Action.ActionID ActionType m)
  }

-- | Implement 'ServerCaps' with Cicero as the job engine
--
-- Jobs are submitted as new @plutus-certification/generate-flake@ facts.
ciceroServerCaps :: forall c m r . (MonadMask m, HasClient c Cicero.API) => EventBackend m r RunClientSelector -> CiceroCaps c m r -> ServerCaps m r
ciceroServerCaps backend CiceroCaps {..} = ServerCaps {..}
  where
    submitJob mods _ ref = RunID . (.id.uuid) <$> runClientOrDie clientCaps backend' req
      where
        backend' = modifyEventBackend mods backend
        uri = ref.uri -- aesonQQ's parser doesn't support RecordDot yet
        req = ciceroClient.fact.create $ Cicero.Fact.CreateFact
          { fact = [aesonQQ| { "plutus-certification/generate-flake": { "ref": #{uriToString id uri ""} } } |]
          , artifact = Nothing
          }

    getRuns mods rid = go 0
      where
        eb = modifyEventBackend mods backend
        rid' = Cicero.Fact.FactID $ rid.uuid
        limit = 10
        go offset = do
          runs <- lift . runClientOrDie clientCaps eb $ ciceroClient.run.getAll True [rid'] (Just offset) (Just limit)
          count <- yieldMany runs .| execStateC 0 (status eb)
          when (count == limit) $ go (offset + limit)
    abortRuns mods rid = runConduit $ go 0
      where
        eb = modifyEventBackend mods backend
        rid' = Cicero.Fact.FactID $ rid.uuid
        limit = 10
        go offset = do
          runs <- lift . runClientOrDie clientCaps eb $ ciceroClient.run.getAll True [rid'] (Just offset) (Just limit)
          let count = List.genericLength runs
          yieldMany runs
            .| filterC (isNothing . (.finishedAt))
            .| mapM_C (abort eb)
          when (count == limit) $ go (offset + limit)
    abort eb = void
        . runClientOrDie clientCaps eb
        . ciceroClient.run.abort
        . (.nomadJobId)

    getLogs mods actionTypeM rid = do
      -- TODO: make a decision if there will be more then 3 runs
      runs <- lift . runClientOrDie clientCaps eb $ ciceroClient.run.getAll True [rid'] Nothing Nothing
      sorted <- List.sort <$> ( lift . runConduit
        $ yieldMany runs
        .| mapMC actionTypeAndRunId
        .| filterC (actionTypeCond . fst)
        .| sinkList
        )
      yieldMany sorted .| mapC snd .| getLogs'
      where
        actionTypeCond (Known actionType) = isNothing actionTypeM || (Just actionType) == actionTypeM
        actionTypeCond Unknown = False

        getLogs' :: ConduitT Cicero.Run.RunID Cicero.Run.RunLog m ()
        getLogs' = await >>= \case
          Nothing -> pure ()
          Just jobId -> do
            (Cicero.Run.RunLogs xs) <- lift . runClientOrDie clientCaps eb $ ciceroClient.run.getLogs jobId
            yieldMany xs
            getLogs'

        actionTypeAndRunId :: Cicero.Run.RunV2 -> m (ActionType,Cicero.Run.RunID)
        actionTypeAndRunId r = do
          inv <- runClientOrDie clientCaps eb $ ciceroClient.invocation.get r.invocationId
          (,r.nomadJobId) . getActionType <$> (runClientOrDie clientCaps eb $ ciceroClient.action.get inv.actionId)
        eb = modifyEventBackend mods backend
        rid' = Cicero.Fact.FactID $ rid.uuid
    status eb = await >>= \case
      Nothing -> pure ()
      Just r ->  do
        modify (+ 1)
        inv <- lift . lift $
          runClientOrDie clientCaps eb $ ciceroClient.invocation.get r.invocationId
        ty <- lift . lift $ actionCache.lookup inv.actionId >>= \case
          Just ty -> pure ty
          Nothing -> do
            act <- runClientOrDie clientCaps eb $ ciceroClient.action.get inv.actionId
            let ty = getActionType act
            actionCache.register inv.actionId ty
            pure ty
        case ty of
          Unknown -> pure ()
          Known s -> yieldM . lift $ case s of
            Generate -> Incomplete <$>
              (getIntermediateOutput eb r "plutus-certification/generate-flake" <&> \case
                Nothing -> Preparing Running
                Just False -> Preparing Failed
                Just True -> Building Running)
            Build -> Incomplete <$>
              (getIntermediateOutput eb r "plutus-certification/build-flake" <&> \case
                Nothing -> Building Running
                Just False -> Building Failed
                Just True -> Certifying $ CertifyingStatus Running Nothing Nothing)
            Certify ->
              getCertifyOutput eb r "plutus-certification/run-certify" <&> \case
                (Nothing, plan) -> Incomplete . Certifying $ CertifyingStatus Running Nothing plan
                (Just (Intermediate p), plan) -> Incomplete . Certifying $ CertifyingStatus Running (Just p) plan
                (Just CertifyFailed, plan) -> Incomplete . Certifying $ CertifyingStatus Failed Nothing plan
                (Just (Succeeded cr), _) -> Finished cr
        status eb

    getRunFacts eb r =
      runClientOrDie clientCaps eb $ ciceroClient.fact.getAll r.nomadJobId

    getIntermediateOutput eb r name = if isJust r.finishedAt
      then do
        facts <- getRunFacts eb r
        let getOutput' (Object o)
              | Just (Object out) <- KM.lookup name o
              , Just _ <- KM.lookup "success"  out = Just True
              | Just (Object out) <- KM.lookup name o
              , Just _ <- KM.lookup "failure"  out = Just False
              | otherwise = Nothing
            getOutput' _ = Nothing
        pure . getFirst . foldMap (First . getOutput' . (.value)) $ facts
      else pure Nothing

    progressLater _ Nothing = True
    progressLater p (Just (Intermediate p')) = p.progressIndex > p'.progressIndex
    progressLater _ _ = False

    getCertifyOutput eb r name = do
      facts <- getRunFacts eb r
      pure $ foldr (\f (acc, plan) -> case f.value of
                       Object o -> case KM.lookup name o of
                         Nothing -> (acc, plan)
                         Just v -> case fromJSON v of
                           Success (I.Plan plan') -> (acc, Just plan')
                           Success (I.Status p) -> if progressLater p acc
                             then (Just $ Intermediate p, plan)
                             else (acc, plan)
                           Success (I.Success cr) -> (Just $ Succeeded cr, plan)
                           Error _ -> if
                             | Just (Object out) <- KM.lookup name o
                             , Just _ <- KM.lookup "failure"  out -> (Just CertifyFailed, plan)
                             | otherwise -> (acc, plan)
                       _ -> (acc, plan)
                   ) (Nothing, Nothing) facts

    getActionType :: Cicero.Action.ActionV2 -> ActionType
    getActionType act
      | act.name == "plutus-certification/generate-flake" = Known Generate
      | act.name == "plutus-certification/build-flake" = Known Build
      | act.name == "plutus-certification/run-certify" = Known Certify
      | otherwise = Unknown

renderRunClientSelector :: RenderSelectorJSON RunClientSelector
renderRunClientSelector RunClient = ("running-client", clientErrorJSON)
