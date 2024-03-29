{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Applicative
import IOHK.Certification.Actions
import IOHK.Certification.Interface
import System.FilePath
import System.IO
import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Conduit
import Control.Monad.Trans.Resource

data Args = Args
  { certifyPath :: !FilePath
  , certifyArgs :: !CertifyArgs
  } deriving (Show)

argsParser :: Parser Args
argsParser =  Args
          <$> strArgument
              ( metavar "BUILD_OUT"
             <> help "the path to the output from build-flake"
              )
          <*> parseCertifyArgs

argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
 <> header "run-certify — Run the certification binary"
  )

printMessage :: ConduitT (Either Text Message) Void ResIO ()
printMessage = await >>= \case
  Nothing -> pure ()
  Just m -> do
    liftIO . BSL8.putStrLn $ encode m
    printMessage

main :: IO ()
main = do
  Args {..} <- execParser argsInfo
  hSetBuffering stdout LineBuffering
  runConduitRes $ runCertifyInProcess () certifyArgs certifyPath .| printMessage
