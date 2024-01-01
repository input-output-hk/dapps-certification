{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Plutus.Certification.Server.Kube (kubeServerCaps, BadStatusLine(..), CertifyFailed(..)) where

import Conduit
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.Trans.Resource
import Data.Aeson (fromJSON)
import qualified Data.Aeson.Types as Aeson
import Data.ByteString
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Conduit.Aeson
import Data.Conduit.Binary as DCB
import Data.Functor
import Data.Text as T
import qualified Data.Text.IO as T
import IOHK.Certification.Actions
import IOHK.Certification.Interface
import Observe.Event
import Paths_plutus_certification
import Plutus.Certification.API.Routes (RunIDV1(..))
import Plutus.Certification.Server
import System.FilePath
import System.IO
import System.Process.Typed
import Text.Mustache
import Text.Regex
import UnliftIO.Exception

data TemplateParams = TemplateParams
  { runCertifyImage :: !Text
  , runId :: !RunIDV1
  , certifyArgs :: !CertifyArgs
  , certifyPath :: !FilePath
  } deriving (Eq, Show)

instance ToMustache TemplateParams where
  toMustache (TemplateParams {..}) = object
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

data CertifyJobHandle = CertifyJobHandle
  { k8sHandle :: !KubeResourceHandle
  , jobState :: !(MVar CertifyJobState)
  }

-- | kubectl output format to get the job status
statusFormat :: String
statusFormat = "jsonpath={.status.ready}{\" \"}{.status.succeeded}{\" \"}{.status.failed}{\"\\n\"}"

-- | Regex to match the kubectl job status output
statusRegex :: Regex
statusRegex = mkRegex "([[:digit:]]?) ([[:digit:]]?) ([[:digit:]]?)"

-- | Exception for when @kubectl get@ outputs an unexpectedly formatted line
newtype BadStatusLine = BadStatusLine
  { line :: ByteString
  } deriving Show
instance Exception BadStatusLine

acquireCertifyJob :: Text -> Acquire CertifyJobHandle
acquireCertifyJob cfg = do
  k8sHandle@(KubeResourceHandle {..}) <- acquireKubeResource cfg
  let
    -- Watch the kube job status in a loop
    statusCmd = setStdout createPipe
              $ proc "kubectl"
                     [ "get"
                     , resourceName
                     , "-o"
                     , statusFormat
                     , "-w"
                     ]
  jobState <- liftIO newEmptyMVar
  statusProc <- acquireProcessWait statusCmd
  let
    readStatus = await >>= \case
      Nothing -> pure ()
      Just line -> do
        let
          lineChars = BS.unpack line
          lineParse = matchRegex statusRegex lineChars
        case lineParse of
          Just ("0" : "" : "" : []) -> readStatus
          Just ("1" : "" : "" : []) -> do
            void . liftIO $ tryPutMVar jobState Running
            readStatus
          Just ("0" : "1" : "" : []) -> do
            void . liftIO $ tryTakeMVar jobState
            liftIO $ putMVar jobState Succeeded
            stopProcess statusProc
          Just ("0" : "" : "1" : []) -> do
            void . liftIO $ tryTakeMVar jobState
            liftIO $ putMVar jobState Failed
            stopProcess statusProc
          _ -> do
            stopProcess statusProc
            throwIO $ BadStatusLine line
  -- Consume the status line in the background, updating the job state mvar as needed
  void $ mkAcquire
    (async $ onException
     (runConduitRes $ sourceHandle (getStdout statusProc) .| DCB.lines .| readStatus)
     (void (tryTakeMVar jobState) >> putMVar jobState Failed))
    cancel
  pure $ CertifyJobHandle {..}

getJobState :: CertifyJobHandle -> IO CertifyJobState
getJobState (CertifyJobHandle {..}) = readMVar jobState

streamJobLogs :: CertifyJobHandle -> ConduitT () ByteString ResIO ()
streamJobLogs j@(CertifyJobHandle {..}) = do
  let logCmd = setStdout createPipe
             $ proc "kubectl" [ "logs", "-f", resourceName k8sHandle ]
  -- Wait for the job to be at least ready
  void . liftIO $ getJobState j
  (_, logProc) <- allocateAcquire $ acquireProcessWait logCmd
  sourceHandle $ getStdout logProc

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
      liftIO (getJobState jobHandle) >>= \case
        Succeeded -> pure ()
        Failed -> throwIO CertifyFailed
        _ -> pure ()
        -- TODO: There's a race here: The job state may not be updated by the
        -- time the logs complete. We don't know if this is failure or success,
        -- unless we check for the final certification result.

-- | 'ServerCaps' that run the certification job in a kubernetes batch job
kubeServerCaps :: EventBackend IO r IOServerSelector
               -> Text -- ^ The Docker image with run-certify
               -> IO (ServerCaps IO r)
kubeServerCaps be runCertifyImage = runCertifyKube runCertifyImage >>= ioServerCaps be
