{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plutus.Certification.Server
  ( module X
  , IOServerSelector(..)
  , renderIOServerSelector
  , ioServerCaps
  ) where

import Plutus.Certification.Server.Instance as X
import Plutus.Certification.Server.Internal as X
  ( ServerCaps(..)
  , ServerEventSelector(..)
  , renderServerEventSelector
  , ensureProfile
  , ensureProfileFromBs
  )

import Plutus.Certification.API
import Observe.Event.Render.JSON
import Observe.Event
import Observe.Event.BackendModification
import IOHK.Certification.Interface
import IOHK.Certification.Actions
import Data.Functor
import Data.UUID
import Data.UUID.V4
import Data.Coerce
import Data.Aeson hiding (Success)
import Network.URI
import Conduit
import System.FilePath
import System.IO.Temp
import Control.Concurrent.Async

import Data.Time
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Exception

import Control.Monad
import Data.Maybe (isNothing)

data JobState = JobState
  { status :: !(Maybe [CertificationTask] -> RunStatusV1)
  , plan :: !(Maybe [CertificationTask])
  , logs :: !(Logs T.Text)
  }

emptyLogs :: Logs a
emptyLogs = Logs [] [] []

emptyJobState :: JobState
emptyJobState = JobState (const $ Incomplete Queued) Nothing emptyLogs

setPlan :: [CertificationTask] -> JobState -> JobState
setPlan p js = js { plan = Just p }

getStatus' :: JobState -> RunStatusV1
getStatus' js = status js $ plan js

type LogEntry a = (ZonedTime,a)
data Logs a = Logs
  { generate :: ![LogEntry a]
  , build :: ![LogEntry a]
  , certify :: ![LogEntry a]
  }

addLog:: CertificationStage -> LogEntry T.Text -> JobState -> JobState
addLog actionType val js@JobState{..} = js { logs = newLogs}
  where
  newLogs = case actionType of
    Generate -> logs { generate = val:(logs.generate)}
    Build -> logs { build = val:(logs.build)}
    Certify -> logs { certify = val:(logs.certify)}

ioServerCaps :: EventBackend IO r IOServerSelector
             -> RunCertify RunIDV1 IO
             -> IO (ServerCaps IO r)
ioServerCaps backend runCertify = do
  jobs <- newIORef Map.empty
  cancellations <- newIORef Map.empty
  let
    freeCancellation jobId = atomicModifyIORef' cancellations (\rs -> (Map.delete jobId rs, ()))
    addCancellation jobId run= atomicModifyIORef' cancellations (\rs -> (Map.insert jobId (cancel run) rs, ()))
    submitJob mods certifyArgs ghAccessTokenM  (FlakeRef uri) = withEvent (modifyEventBackend mods backend) SubmitJob \ev -> do
      addField ev $ SubmittedRef uri

      jobId <- nextRandom
      addField ev $ JobID jobId

      atomicModifyIORef' jobs (\js -> (Map.insert jobId emptyJobState js, ()))

      let
        changeJobState :: (JobState -> JobState) -> IO ()
        changeJobState f = atomicModifyIORef' jobs (\js -> (Map.adjust f jobId js, ()))

        setStatus :: (Maybe [CertificationTask] -> RunStatusV1) -> IO ()
        setStatus st = changeJobState (\s -> s { status = st })

        addLogEntry :: CertificationStage -> T.Text -> IO ()
        addLogEntry actionType text = do
          time <- utcToZonedTime utc <$> getCurrentTime
          changeJobState (addLog actionType (time,text))

      let runJob = withSubEvent ev RunningJob \rEv -> withSystemTempDirectory "generate-flake" \dir -> do
            addField rEv $ TempDir dir
            setStatus . const $ Incomplete (Preparing Running)
            catch
              (generateFlake (narrowEventBackend InjectGenerate $
                subEventBackend rEv) (addLogEntry Generate) ghAccessTokenM uri dir)
              (\(ex :: SomeException) -> do
                addLogEntry Generate (T.pack $ show ex)
                setStatus . const $ Incomplete (Preparing Failed)
              )
            setStatus . const $ Incomplete (Building Running)
            certifyOut <- onException
              (buildFlake (narrowEventBackend InjectBuild $ subEventBackend rEv) (addLogEntry Build) ghAccessTokenM dir)
              (setStatus . const $ Incomplete (Building Failed))
            setStatus $ \p -> Incomplete (Certifying (CertifyingStatus Running Nothing p))
            let go = await >>= \case
                  Just (Right (Success res)) -> liftIO $ setStatus . const $ Finished res
                  Just (Right (Status pr)) -> do
                    liftIO . setStatus $ \pl -> Incomplete (Certifying $ CertifyingStatus Running (Just pr) pl)
                    go
                  Just (Right (Plan p)) -> do
                    liftIO $ changeJobState (setPlan p)
                    go
                  Just (Left l) -> do
                    liftIO $ addLogEntry Certify l
                    go
                  Nothing -> pure ()
            onException
              (runConduitRes $ runCertify (RunID jobId) certifyArgs (certifyOut </> "bin" </> "certify") .| go)
              (setStatus $ \pl -> Incomplete (Certifying $ CertifyingStatus Failed Nothing pl)) -- TODO get latest actual status update

      async ( finally runJob (freeCancellation jobId)) >>= addCancellation jobId
      pure $ coerce jobId

    getStatus _ (RunID jobId) = readIORef jobs
      <&> getStatus' . Map.findWithDefault emptyJobState jobId

    abortRuns mods rid@(RunID jobId) = withEvent (modifyEventBackend mods backend) AbortJob \ev -> do
     addField ev rid
     readIORef cancellations >>= sequence_ . Map.lookup jobId
     freeCancellation jobId

    getLogs _ actionTypeM (RunID jobId) = do
      (JobState _ _ (Logs generate build certify)) <-
        Map.findWithDefault emptyJobState jobId <$> lift (readIORef jobs)

      whenMatches Generate yield' generate
      whenMatches Build yield' build
      whenMatches Certify yield' certify

      where
        yield' logs actionType =
          yieldMany (reverse logs) .| mapC (toRunLog (source actionType))

        whenMatches actionType yieldLogs logs = when
          (isNothing actionTypeM || actionTypeM == Just actionType)
          (yieldLogs logs actionType)

        toRunLog src (ztime,text) = RunLog ztime src text

        source Generate ="plutus-certification/generate-flake"
        source Build    ="plutus-certification/build-flake"
        source Certify  ="plutus-certification/run-certify"

  pure $ ServerCaps {..}

data IOServerSelector f where
  InjectGenerate :: forall f . GenerateFlakeSelector f -> IOServerSelector f
  InjectBuild :: forall f . BuildFlakeSelector f -> IOServerSelector f
  SubmitJob :: IOServerSelector SubmitJobField
  RunningJob :: IOServerSelector RunningJobField
  AbortJob :: IOServerSelector RunIDV1

renderIOServerSelector :: RenderSelectorJSON IOServerSelector
renderIOServerSelector (InjectGenerate s) = ( "generate-flake:" <> k
                                         , renderField
                                         )
  where
    (k, renderField) = renderGenerateFlakeSelector s
renderIOServerSelector SubmitJob = ("submit-job", renderSubmitJobField)
renderIOServerSelector RunningJob = ("running-job", renderRunningJobField)
renderIOServerSelector (InjectBuild s) = ( "build-flake:" <> k
                                      , renderField
                                      )
  where
    (k, renderField) = renderBuildFlakeSelector s
renderIOServerSelector AbortJob = ("abort-job", \rid -> ("run-id",toJSON rid))


data SubmitJobField = SubmittedRef !URI
                    | JobID !UUID
renderSubmitJobField :: RenderFieldJSON SubmitJobField
renderSubmitJobField (SubmittedRef u) = ("submitted-ref", toJSON $ uriToString id u "")
renderSubmitJobField (JobID jobId) = ("job-id", toJSON jobId)

newtype RunningJobField = TempDir FilePath

renderRunningJobField :: RenderFieldJSON RunningJobField
renderRunningJobField (TempDir p) = ("temp-dir", toJSON p)
