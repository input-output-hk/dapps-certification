{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Applicative
import IOHK.Certification.Actions
import IOHK.Certification.Interface
import System.FilePath
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Conduit
import Control.Monad.Trans.Resource

data Args = Args
  { buildOut :: !FilePath
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

data PrefixJSON a = PrefixJSON [ Key ] a

instance (ToJSON a) => ToJSON (PrefixJSON a) where
  toJSON (PrefixJSON [] a) = toJSON a
  toJSON (PrefixJSON (hd : tl) a) = object [ hd .= PrefixJSON tl a ]
  toEncoding (PrefixJSON [] a) = toEncoding a
  toEncoding (PrefixJSON (hd : tl) a) = pairs ( hd .= PrefixJSON tl a )

printMessage :: ConduitT Message Void ResIO ()
printMessage = await >>= \case
  Nothing -> pure ()
  Just m -> do
    liftIO . BSL8.putStrLn . encode $ PrefixJSON [ "plutus-certification/run-certify" ] m
    printMessage

main :: IO ()
main = do
  ss@Args {..} <- execParser argsInfo
  putStrLn $ "Running certify with args: " <> show ss
  let noLogExtraction = const $ pure ()
      certifyPath = buildOut </> "bin" </> "certify"
  runConduitRes $ runCertify noLogExtraction certifyArgs certifyPath .| printMessage
