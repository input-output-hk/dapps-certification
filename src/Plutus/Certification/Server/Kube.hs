{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Plutus.Certification.Server.Kube (kubeServerCaps) where

import Data.Text
import Development.Placeholders
import IOHK.Certification.Actions
import IOHK.Certification.Interface
import Observe.Event
import Paths_plutus_certification
import Plutus.Certification.API.Routes
import Plutus.Certification.Server
import System.FilePath
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
      $(todo "launch job")

-- | 'ServerCaps' that run the certification job in a kubernetes batch job
kubeServerCaps :: EventBackend IO r IOServerSelector
               -> Text -- ^ The Docker image with run-certify
               -> IO (ServerCaps IO r)
kubeServerCaps be runCertifyImage = runCertifyKube runCertifyImage >>= ioServerCaps be
