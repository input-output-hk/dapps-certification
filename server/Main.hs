{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Servant
import Network.Wai.Handler.Warp
import Options.Applicative
import Data.Function

import Plutus.Certification.API
import Paths_plutus_certification qualified as Package

server :: Server API
server = NamedAPI
  { version = pure $ VersionV1 Package.version
  , versionHead = pure NoContent
  }

app :: Application
app = serve (Proxy @API) server

data Args = Args
  { port :: !Port
  , host :: !HostPreference
  }

args :: Parser Args
args =  Args
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "server port number"
     <> showDefault
     <> value 9671
      )
  <*> option auto
      ( long "bind"
     <> short 'b'
     <> metavar "HOST"
     <> help "server bind address"
     <> showDefault
     <> value "*6"
      )

opts :: ParserInfo Args
opts = info (args <**> helper)
  ( fullDesc
 <> progDesc "Run the plutus-certification server"
 <> header "plutus-certification — Certification as a service for Plutus applications"
  )

main :: IO ()
main = do
  Args { .. } <- execParser opts
  let settings = defaultSettings
               & setPort port
               & setHost host
  runSettings settings app
