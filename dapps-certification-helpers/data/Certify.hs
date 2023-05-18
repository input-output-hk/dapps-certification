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
import "directory" System.Directory
import "bytestring" Data.ByteString.Lazy.Char8 qualified as BSL8
import "base" GHC.Stack
import "plutus-contract-certification" Plutus.Contract.Test.Certification.Run
import "plutus-contract" Plutus.Contract.Test.Coverage
import "base" System.IO
import "lens" Control.Lens
import "dapps-certification-interface" IOHK.Certification.Interface qualified as I
import "unix" System.Posix.IO
import "plutus-tx" PlutusTx.Coverage
import "uuid" Data.UUID.V4

taskName :: CertificationTask -> I.CertificationTaskName
taskName UnitTestsTask = I.UnitTestsTask
taskName StandardPropertyTask = I.StandardPropertyTask
-- taskName DoubleSatisfactionTask = I.DoubleSatisfactionTask
taskName NoLockedFundsTask = I.NoLockedFundsTask
taskName NoLockedFundsLightTask = I.NoLockedFundsLightTask
taskName CrashToleranceTask = I.CrashToleranceTask
taskName WhitelistTask = I.WhitelistTask
taskName DLTestsTask = I.DLTestsTask
taskName t = I.UnknownTask $ show t

translateCertificationTask :: CertificationTask -> I.CertificationTask
translateCertificationTask t = I.CertificationTask (taskName t) (fromEnum t)

data Skippable a
  = Skippable !a
  | Unskippable !a

unskip :: Skippable a -> a
unskip (Skippable x) = x
unskip (Unskippable x) = x

-- Assumes single writer!
type MessageChannel = MVar (Maybe (Skippable I.Message))

withMessageChannel :: Handle -> (MessageChannel -> IO a) -> IO a
withMessageChannel h go = do
    latestMessage <- newEmptyMVar
    fst <$> concurrently (go latestMessage `finally` closeMessageChannel latestMessage) (consume latestMessage)
  where
    consume latestMessage = takeMVar latestMessage >>= \case
      Nothing -> hClose h
      Just msg -> do
        BSL8.hPutStrLn h . encode $ unskip msg
        consume latestMessage

data EmitAfterClose = EmitAfterClose deriving Show

instance Exception EmitAfterClose

emitMessage' :: MessageChannel -> Skippable I.Message -> IO ()
emitMessage' latestMessage msg = do
  tryTakeMVar latestMessage >>= \case
    Just Nothing -> throwIO EmitAfterClose
    Just v@(Just (Unskippable _)) -> do
      putMVar latestMessage v
    _ -> pure ()
  putMVar latestMessage $ Just msg

emitSkippableMessage :: MessageChannel -> I.Message -> IO ()
emitSkippableMessage latestMessage = emitMessage' latestMessage . Skippable

emitMessage :: MessageChannel -> I.Message -> IO ()
emitMessage latestMessage = emitMessage' latestMessage . Unskippable

closeMessageChannel :: MessageChannel -> IO ()
closeMessageChannel = flip putMVar Nothing

postProgress :: HasCallStack => Chan CertificationEvent -> MessageChannel -> IO ()
postProgress eventChan msgChan = readEvents initState
  where
    readEvents :: HasCallStack => I.Progress -> IO ()
    readEvents st = readChan eventChan >>= \case
      CertificationDone -> pure ()
      ev -> do
        let st' = updateState ev st
        emitSkippableMessage msgChan $ I.Status st'
        readEvents st'

    initState = I.Progress
      { currentTask = Nothing
      , currentQc = newQc
      , finishedTasks = mempty
      , progressIndex = 0
      }

    newQc = I.QCProgress 0 0 0 Nothing

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
    updateState (QuickCheckNumTestsEvent ct) st = st
      { I.currentQc = (I.currentQc st) { I.qcExpected = Just ct }
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

getHtmlReport :: CoverageReport -> IO String
getHtmlReport rep = do
  uuid <- show <$> nextRandom
  writeCoverageReport uuid rep
  let fn = uuid ++ ".html"
  readFile fn <* removeFile fn

main :: HasCallStack => IO ()
main = do
  out <- dup stdOutput
  _ <- dupTo stdError stdOutput

  eventChan <- newChan

  h <- fdToHandle out
  hSetBuffering h LineBuffering

  withMessageChannel h $ \msgChan -> do
    emitMessage msgChan . I.Plan $ translateCertificationTask <$> certificationTasks certification

    let certOpts = defaultCertificationOptions
          { certOptOutput = False
          , certEventChannel = Just eventChan
          }
    (res, _) <- concurrently (certifyWithOptions certOpts certification) (postProgress eventChan msgChan)
    report <- getHtmlReport (res ^. certRes_coverageReport)

    emitMessage msgChan . I.Success $ I.CertificationResult (res,report)
