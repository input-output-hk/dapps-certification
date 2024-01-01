{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Plutus.Certification.Server.Kube (kubeServerCaps) where

import Control.Concurrent.Async
import Data.Acquire
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor
import Data.Text
import qualified Data.Text.IO as T
import Development.Placeholders
import IOHK.Certification.Actions
import IOHK.Certification.Interface
import Observe.Event
import Paths_plutus_certification
import Plutus.Certification.API.Routes
import Plutus.Certification.Server
import System.FilePath
import System.IO
import System.Process.Typed
import Text.Mustache
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
      (_, jobHandle) <- allocateAcquire $ acquireKubeResource jobConfig
      $(todo "stream job logs")

-- | 'ServerCaps' that run the certification job in a kubernetes batch job
kubeServerCaps :: EventBackend IO r IOServerSelector
               -> Text -- ^ The Docker image with run-certify
               -> IO (ServerCaps IO r)
kubeServerCaps be runCertifyImage = runCertifyKube runCertifyImage >>= ioServerCaps be
