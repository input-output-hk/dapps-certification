{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Plutus.Certification.Local where

import Plutus.Certification.Server
import Plutus.Certification.API
import Observe.Event.Render.JSON
import Observe.Event
import Observe.Event.BackendModification
import IOHK.Certification.Interface
import IOHK.Certification.Interface.Actions
import Data.UUID
import Data.UUID.V4
import Data.Coerce
import Data.Aeson hiding (Success)
import Network.URI
import Conduit
import System.FilePath
import System.IO.Temp
import Control.Concurrent.Async
import Data.IORef
import qualified Data.Map.Strict as Map
import Control.Exception

localServerCaps :: EventBackend IO r LocalSelector -> IO (ServerCaps IO r)
localServerCaps backend = do
  jobs <- newIORef Map.empty
  let
    submitJob mods (FlakeRef uri) = withEvent (modifyEventBackend mods backend) SubmitJob \ev -> do
      addField ev $ SubmittedRef uri

      jobId <- nextRandom
      addField ev $ JobID jobId

      atomicModifyIORef' jobs (\js -> (Map.insert jobId [] js, ()))

      let
        addStatus :: RunStatusV1 -> IO ()
        addStatus st = atomicModifyIORef' jobs (\js -> (Map.adjust (st :) jobId js, ()))

      -- Purposefully leak...
      _ <- async $ withSubEvent ev RunningJob \rEv -> withSystemTempDirectory "generate-flake" \dir -> do
        addField rEv $ TempDir dir
        addStatus $ Incomplete (Preparing Running)
        onException
          (generateFlake (narrowEventBackend InjectGenerate $ subEventBackend rEv) uri dir)
          (addStatus $ Incomplete (Preparing Failed))
        addStatus $ Incomplete (Building Running)
        certifyOut <- onException
          (buildFlake (narrowEventBackend InjectBuild $ subEventBackend rEv) dir)
          (addStatus $ Incomplete (Building Failed))
        addStatus $ Incomplete (Certifying Running Nothing)
        let go = await >>= \case
              Just (Success res) -> liftIO $ addStatus $ Finished res
              Just (Status p) -> do
                liftIO . addStatus $ Incomplete (Certifying Running (Just p))
                go
              Nothing -> pure ()
        onException
          (runConduitRes $ runCertify (certifyOut </> "bin" </> "certify") .| go)
          (addStatus $ Incomplete (Certifying Failed Nothing)) -- TODO get latest actual status update
      pure $ coerce jobId

    getRuns _ (RunID jobId) =
      Map.findWithDefault [] jobId <$> (lift $ readIORef jobs) >>= yieldMany

  pure $ ServerCaps {..}

data LocalSelector f where
  InjectGenerate :: forall f . GenerateFlakeSelector f -> LocalSelector f
  InjectBuild :: forall f . BuildFlakeSelector f -> LocalSelector f
  SubmitJob :: LocalSelector SubmitJobField
  RunningJob :: LocalSelector RunningJobField

renderLocalSelector :: RenderSelectorJSON LocalSelector
renderLocalSelector (InjectGenerate s) = ( "generate-flake:" <> k
                                         , renderField
                                         )
  where
    (k, renderField) = renderGenerateFlakeSelector s
renderLocalSelector SubmitJob = ("submit-job", renderSubmitJobField)
renderLocalSelector RunningJob = ("running-job", renderRunningJobField)
renderLocalSelector (InjectBuild s) = ( "build-flake:" <> k
                                      , renderField
                                      )
  where
    (k, renderField) = renderBuildFlakeSelector s


data SubmitJobField = SubmittedRef !URI
                    | JobID !UUID
renderSubmitJobField :: RenderFieldJSON SubmitJobField
renderSubmitJobField (SubmittedRef u) = ("submitted-ref", toJSON $ uriToString id u "")
renderSubmitJobField (JobID jobId) = ("job-id", toJSON jobId)

data RunningJobField = TempDir !FilePath

renderRunningJobField :: RenderFieldJSON RunningJobField
renderRunningJobField (TempDir p) = ("temp-dir", toJSON p)
