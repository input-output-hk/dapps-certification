{-# LANGUAGE TemplateHaskell #-}
module Plutus.Certification.Server.Kube where

import Development.Placeholders
import IOHK.Certification.Actions
import Observe.Event
import Plutus.Certification.Server

runCertifyKube :: IO (RunCertify IO)
runCertifyKube = do
  $(todo "Load mustache template")

-- | 'ServerCaps' that run the certification job in a kubernetes batch job
kubeServerCaps :: EventBackend IO r IOServerSelector
               -> IO (ServerCaps IO r)
kubeServerCaps be = runCertifyKube >>= ioServerCaps be
