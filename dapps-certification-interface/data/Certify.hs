{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RecordWildCards     #-}
module Main where

import "certification" Certification (certification)
import "async" Control.Concurrent.Async
import "base" Control.Concurrent.Chan
import "base" Control.Concurrent.MVar
import "base" Control.Exception
import "aeson" Data.Aeson
import "base" Data.Maybe
import "bytestring" Data.ByteString.Lazy.Char8 qualified as BSL8
import "base" GHC.Stack
import "plutus-contract-certification" Plutus.Contract.Test.Certification.Run
import "base" System.IO
import "dapps-certification-interface" IOHK.Certification.Interface qualified as I
import "unix" System.Posix.IO

taskName :: CertificationTask -> I.CertificationTaskName
taskName UnitTestsTask = I.UnitTestsTask
taskName StandardPropertyTask = I.StandardPropertyTask
taskName DoubleSatisfactionTask = I.DoubleSatisfactionTask
taskName NoLockedFundsTask = I.NoLockedFundsTask
taskName NoLockedFundsLightTask = I.NoLockedFundsLightTask
taskName CrashToleranceTask = I.CrashToleranceTask
taskName WhitelistTask = I.WhitelistTask
taskName DLTestsTask = I.DLTestsTask
taskName t = I.UnknownTask $ show t

translateCertificationTask :: CertificationTask -> I.CertificationTask
translateCertificationTask t = I.CertificationTask (taskName t) (fromEnum t)

postProgress :: HasCallStack => Chan CertificationEvent -> Handle -> IO ()
postProgress eventChan h = do
    latestEvent <- newEmptyMVar
    concurrently_ (feed latestEvent) (post latestEvent initState)
  where
    -- Only works if single producer!
    forcePutMVar v x = do
      _ <- tryTakeMVar v
      putMVar v x

    feed latestEvent = do
      ev <- readChan eventChan
      forcePutMVar latestEvent ev
      case ev of
        CertificationDone -> pure ()
        _ -> feed latestEvent

    post :: HasCallStack => MVar CertificationEvent -> I.Progress -> IO ()
    post latestEvent st = takeMVar latestEvent >>= \case
      CertificationDone -> pure ()
      ev -> do
        let st' = updateState ev st
        BSL8.hPutStrLn h . encode $ I.Status st'
        post latestEvent st'

    initState = I.Progress
      { currentTask = Nothing
      , currentQc = newQc
      , finishedTasks = mempty
      , progressIndex = 0
      }

    newQc = I.QCProgress 0 0 0

    updateState :: HasCallStack => CertificationEvent -> I.Progress -> I.Progress
    updateState (QuickCheckTestEvent Nothing) st = st
      { I.currentQc = (I.currentQc st) { I.qcDiscarded = I.qcDiscarded (I.currentQc st) + 1 }
      , I.progressIndex = (I.progressIndex st) + 1
      }
    updateState (QuickCheckTestEvent (Just True)) st = st
      { I.currentQc = (I.currentQc st) { I.qcSuccesses = I.qcSuccesses (I.currentQc st) + 1 }
      , I.progressIndex = (I.progressIndex st) + 1
      }
    updateState (QuickCheckTestEvent (Just False)) st = st
      { I.currentQc = (I.currentQc st) { I.qcFailures = I.qcFailures (I.currentQc st) + 1 }
      , I.progressIndex = (I.progressIndex st) + 1
      }
    updateState (StartCertificationTask ct) st = st
      { I.currentTask = Just $ translateCertificationTask ct
      , I.currentQc = newQc
      , I.progressIndex = (I.progressIndex st) + 1
      }
    updateState (FinishedTask res) st = st
      { I.currentTask = Nothing
      , I.currentQc = newQc
      , I.finishedTasks = (I.TaskResult (fromJust $ I.currentTask st) (I.currentQc st) res) : (I.finishedTasks st)
      , I.progressIndex = (I.progressIndex st) + 1
      }
    updateState CertificationDone _ = error "unreachable"
    updateState _ st = st

main :: HasCallStack => IO ()
main = do
  out <- dup stdOutput
  _ <- dupTo stdError stdOutput

  eventChan <- newChan

  h <- fdToHandle out

  let certOpts = defaultCertificationOptions
        { certOptOutput = False
        , certEventChannel = Just eventChan
        }
  (res, _) <- concurrently (certifyWithOptions certOpts certification) (postProgress eventChan h)

  BSL8.hPutStrLn h . encode . I.Success $ I.CertificationResult res
  hClose h
