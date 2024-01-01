{-# LANGUAGE TemplateHaskell #-}
module Plutus.Certification.Server.Kube (kubeServerCaps) where

import Development.Placeholders
import IOHK.Certification.Actions
import Observe.Event
import Paths_plutus_certification
import Plutus.Certification.API.Routes
import Plutus.Certification.Server
import System.FilePath
import Text.Mustache
import UnliftIO.Exception

runCertifyKube :: IO (RunCertify RunIDV1 IO)
runCertifyKube = do
    dataDir <- (</> "data") <$> getDataDir
    template <- automaticCompile [ dataDir ] "run-certify-k8s.yaml.mustache" >>= fromEither
    pure $ rck template
  where
    rck :: Template -> RunCertify RunIDV1 IO
    rck template jobId certifyArgs certify = do
      $(todo "render template")

-- | 'ServerCaps' that run the certification job in a kubernetes batch job
kubeServerCaps :: EventBackend IO r IOServerSelector
               -> IO (ServerCaps IO r)
kubeServerCaps be = runCertifyKube >>= ioServerCaps be
