{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Plutus.Certification.Server.Kube (kubeServerCaps, BadStatus(..), CertifyFailed(..), StatusLoopEOF(..)) where

import Conduit
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Conduit.Aeson
import Data.Functor
import Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector as V
import IOHK.Certification.Actions
import IOHK.Certification.Interface
import Observe.Event
import Paths_plutus_certification
import Plutus.Certification.API.Routes (RunIDV1(..))
import Plutus.Certification.Server
import System.FilePath
import System.IO
import System.Process.Typed
import Text.Mustache as M
import UnliftIO.Exception

data TemplateParams = TemplateParams
  { runCertifyImage :: !Text
  , runId :: !RunIDV1
  , certifyArgs :: !CertifyArgs
  , certifyPath :: !FilePath
  } deriving (Eq, Show)

instance ToMustache TemplateParams where
  toMustache (TemplateParams {..}) = M.object
    [ "run_certify_image" ~> runCertifyImage
    , "run_id" ~= runId
    , "certify_path" ~> certifyPath
    , "certify_args" ~> certifyArgsToCommandList certifyArgs
    ]

newtype KubeResourceHandle = KubeResourceHandle
  { resourceName :: String
  }

acquireKubeResource :: Text -> Acquire KubeResourceHandle
acquireKubeResource cfg = mkAcquire (KubeResourceHandle . BSL.unpack . BSL.dropEnd 1 <$> readProcessStdout_ createCmd) cleanup
  where
    cleanup (KubeResourceHandle {..}) =
      runProcess_ $ proc "kubectl" [ "delete", resourceName ]
    createCmd = setStdin (textInput cfg)
              $ proc "kubectl" [ "create", "-o", "name", "-f", "-" ]
    textInput val = mkPipeStreamSpec $ \_ h -> do
      void $ async $ do
        T.hPutStr h val
        hClose h
      return ((), hClose h)

data CertifyJobState
  = Running
  | Succeeded
  | Failed

data KubePodState = PodPending | PodReady | PodSucceeded | PodFailed deriving Show
instance FromJSON KubePodState where
  parseJSON = withObject "PodStatus" $ \v -> do
    phase <- v .: "phase"
    if
      | phase == "Pending" -> pure PodPending
      | phase == "Running" -> do
          conditionsV <- v .: "conditions"
          flip (withArray "Conditions") conditionsV $ \conditions -> do
            let go acc = withObject "Condition" $ \condition -> do
                  status <- condition .: "status"
                  ty <- condition .: "type"
                  pure $ if ty == ("Ready" :: Text) && status == ("True" :: Text)
                    then True
                    else acc
            V.foldM go False conditions <&> \case
              True -> PodReady
              False -> PodPending
      | phase == "Succeeded" -> pure PodSucceeded
      | phase == "Failed" -> pure PodFailed
      | otherwise -> fail $ "Unexpected phase " <> T.unpack phase

data CertifyJobHandle = CertifyJobHandle
  { k8sHandle :: !KubeResourceHandle
  , jobState :: !(MVar CertifyJobState)
  }

-- | Exception for when kubernetes reports an invalid status
newtype BadStatus = BadStatus
  { msg :: String
  } deriving Show
instance Exception BadStatus

-- | Exception for when the status loop terminates unexpectedly
data StatusLoopEOF = StatusLoopEOF deriving Show
instance Exception StatusLoopEOF

acquireCertifyJob :: Text -> Acquire CertifyJobHandle
acquireCertifyJob cfg = do
  k8sHandle@(KubeResourceHandle {..}) <- acquireKubeResource cfg
  let
    -- Watch the kube pod status in a loop
    statusCmd = setStdout createPipe
              $ proc "kubectl"
                     [ "get"
                     , resourceName
                     , "-o"
                     , "jsonpath={.status}"
                     , "-w"
                     ]
  jobState <- liftIO newEmptyMVar
  statusProc <- acquireProcessWait statusCmd
  let
    readStatus = await >>= \case
      Just (Right (_, v)) -> case fromJSON v of
        Aeson.Error s -> throwIO $ BadStatus s
        Aeson.Success PodPending -> readStatus
        Aeson.Success PodReady -> do
          void . liftIO $ tryPutMVar jobState Running
          readStatus
        Aeson.Success PodSucceeded -> do
          void . liftIO $ tryTakeMVar jobState
          liftIO $ putMVar jobState Succeeded
          stopProcess statusProc
        Aeson.Success PodFailed -> do
          void . liftIO $ tryTakeMVar jobState
          liftIO $ putMVar jobState Failed
          stopProcess statusProc
      Just (Left e) -> do
        throwIO . BadStatus $ show e
      Nothing -> throwIO StatusLoopEOF

  -- Query the pod status in the background, updating the job state mvar as needed
  void $ mkAcquire
    (async $ onException
     (runConduitRes $ sourceHandle (getStdout statusProc) .| conduitArrayParserNoStartEither skipSpace .| readStatus)
     (void (tryTakeMVar jobState) >> putMVar jobState Failed))
    cancel
  pure $ CertifyJobHandle {..}

getJobState :: CertifyJobHandle -> IO CertifyJobState
getJobState (CertifyJobHandle {..}) = readMVar jobState

-- | True if successful, False otherwise
waitForCompletion :: CertifyJobHandle -> IO Bool
waitForCompletion (CertifyJobHandle {..}) = go
  where
    go = takeMVar jobState >>= \case
      Running -> go
      Succeeded -> pure True
      Failed -> pure False

streamJobLogs :: CertifyJobHandle -> ConduitT () ByteString ResIO ()
streamJobLogs j@(CertifyJobHandle {..}) = do
  let logCmd = setStdout createPipe
             $ proc "kubectl" [ "logs", "-f", resourceName k8sHandle ]
  -- Wait for the job to be at least ready
  void . liftIO $ getJobState j
  (_, logProc) <- allocateAcquire $ acquireProcessWait logCmd
  sourceHandle $ getStdout logProc

-- | The certification process failed
data CertifyFailed = CertifyFailed deriving (Show)
instance Exception CertifyFailed

runCertifyKube :: Text -- ^ The Docker image with run-certify
               -> IO (RunCertify RunIDV1 IO)
runCertifyKube runCertifyImage = do
    dataDir <- (</> "data") <$> getDataDir
    template <- automaticCompile [ dataDir ] "run-certify-k8s.yaml.mustache" >>= fromEither
    pure $ rck template
  where
    rck :: Template -> RunCertify RunIDV1 IO
    rck template runId certifyArgs certifyPath = do
      let jobConfig = substitute template (TemplateParams {..})
      (k, jobHandle) <- allocateAcquire $ acquireCertifyJob jobConfig
      let toMessage = await >>= \case
            Just (Right (_, v)) -> case fromJSON v of
              Aeson.Error s -> fail s
              Aeson.Success m -> do
                yield m
                toMessage
            Just (Left e) -> do
              yield $ Left $ T.pack $ show e
              liftIO $ throwIO e
            Nothing -> release k
      streamJobLogs jobHandle .| conduitArrayParserNoStartEither skipSpace .| toMessage
      liftIO (waitForCompletion jobHandle) >>= \case
        True -> pure ()
        False -> throwIO CertifyFailed

-- | 'ServerCaps' that run the certification job in a kubernetes batch job
kubeServerCaps :: EventBackend IO r IOServerSelector
               -> Text -- ^ The Docker image with run-certify
               -> IO (ServerCaps IO r)
kubeServerCaps be runCertifyImage = runCertifyKube runCertifyImage >>= ioServerCaps be
