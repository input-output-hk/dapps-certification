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
import IOHK.Certification.Actions
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

data JobState = JobState
  { statuses :: ![(Maybe [CertificationTask] -> RunStatusV1)]
  , plan :: !(Maybe [CertificationTask])
  }

emptyJobState :: JobState
emptyJobState = JobState [] Nothing

addStatus :: (Maybe [CertificationTask] -> RunStatusV1) -> JobState -> JobState
addStatus st js = js { statuses = st : (statuses js) }

setPlan :: [CertificationTask] -> JobState -> JobState
setPlan p js = js { plan = Just p }

getStatuses :: JobState -> [RunStatusV1]
getStatuses js = map (\f -> f $ plan js) $ statuses js

localServerCaps :: EventBackend IO r LocalSelector -> IO (ServerCaps IO r)
localServerCaps backend = do
  jobs <- newIORef Map.empty
  cancellations <- newIORef Map.empty
  let
    freeCancellation jobId = atomicModifyIORef' cancellations (\rs -> (Map.delete jobId rs, ()))
    addCancellation jobId run= atomicModifyIORef' cancellations (\rs -> (Map.insert jobId (cancel run) rs, ()))
    submitJob mods (FlakeRef uri) = withEvent (modifyEventBackend mods backend) SubmitJob \ev -> do
      addField ev $ SubmittedRef uri

      jobId <- nextRandom
      addField ev $ JobID jobId

      atomicModifyIORef' jobs (\js -> (Map.insert jobId emptyJobState js, ()))

      let
        addStatus' :: (Maybe [CertificationTask] -> RunStatusV1) -> IO ()
        addStatus' st = atomicModifyIORef' jobs (\js -> (Map.adjust (addStatus st) jobId js, ()))

      -- Purposefully leak...
      let runJob = withSubEvent ev RunningJob \rEv -> withSystemTempDirectory "generate-flake" \dir -> do
            addField rEv $ TempDir dir
            addStatus' . const $ Incomplete (Preparing Running)
            onException
              (generateFlake (narrowEventBackend InjectGenerate $ subEventBackend rEv) uri dir)
              (addStatus' . const $ Incomplete (Preparing Failed))
            addStatus' . const $ Incomplete (Building Running)
            certifyOut <- onException
              (buildFlake (narrowEventBackend InjectBuild $ subEventBackend rEv) dir)
              (addStatus' . const $ Incomplete (Building Failed))
            addStatus' $ \p -> Incomplete (Certifying (CertifyingStatus Running Nothing p))
            let go = await >>= \case
                  Just (Success res) -> liftIO $ addStatus' . const $ Finished res
                  Just (Status pr) -> do
                    liftIO . addStatus' $ \pl -> Incomplete (Certifying $ CertifyingStatus Running (Just pr) pl)
                    go
                  Just (Plan p) -> do
                    liftIO $ atomicModifyIORef' jobs (\js -> (Map.adjust (setPlan p) jobId js, ()))
                    go
                  Nothing -> pure ()
            onException
              (runConduitRes $ runCertify (certifyOut </> "bin" </> "certify") .| go)
              (addStatus' $ \pl -> Incomplete (Certifying $ CertifyingStatus Failed Nothing pl)) -- TODO get latest actual status update

      (async $ finally runJob (freeCancellation jobId)) >>= addCancellation jobId
      pure $ coerce jobId

    getRuns _ (RunID jobId) =
      getStatuses <$> Map.findWithDefault emptyJobState jobId <$> (lift $ readIORef jobs) >>= yieldMany

    abortRuns mods rid@(RunID jobId) = withEvent (modifyEventBackend mods backend) AbortJob \ev -> do
     addField ev rid
     readIORef cancellations >>= sequence_ . Map.lookup jobId
     freeCancellation jobId

    -- TODO: implement for Local also
    getLogs = \_ _ _ -> yieldMany []

  pure $ ServerCaps {..}

data LocalSelector f where
  InjectGenerate :: forall f . GenerateFlakeSelector f -> LocalSelector f
  InjectBuild :: forall f . BuildFlakeSelector f -> LocalSelector f
  SubmitJob :: LocalSelector SubmitJobField
  RunningJob :: LocalSelector RunningJobField
  AbortJob :: LocalSelector RunIDV1

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
renderLocalSelector AbortJob = ("abort-job", \rid -> ("run-id",toJSON rid))


data SubmitJobField = SubmittedRef !URI
                    | JobID !UUID
renderSubmitJobField :: RenderFieldJSON SubmitJobField
renderSubmitJobField (SubmittedRef u) = ("submitted-ref", toJSON $ uriToString id u "")
renderSubmitJobField (JobID jobId) = ("job-id", toJSON jobId)

data RunningJobField = TempDir !FilePath

renderRunningJobField :: RenderFieldJSON RunningJobField
renderRunningJobField (TempDir p) = ("temp-dir", toJSON p)
