module Plutus.Certification.Local where

import IOHK.Certification.Actions
import Observe.Event
import Plutus.Certification.Server

localServerCaps :: EventBackend IO r IOServerSelector
                -> IO (ServerCaps IO r)
localServerCaps = flip ioServerCaps runCertifyInProcess
