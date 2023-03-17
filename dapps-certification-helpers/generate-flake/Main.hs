{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.URI
import Options.Applicative
import IOHK.Certification.Actions
import Observe.Event
import Observe.Event.Render.JSON
import Observe.Event.Render.IO.JSON
import System.Directory
import Data.Aeson
import IOHK.Certification.Interface

data Args = Args
  { flakeref :: !URI
  , output :: !FilePath
  , githubToken :: !(Maybe GitHubAccessToken)
  }

-- TODO Deduplicate with certification-client
flakeRefReader :: ReadM URI
flakeRefReader = do
  urlStr <- str
  case parseAbsoluteURI urlStr of
    Just u -> case uriScheme u of
      "github:" -> pure u
      s -> readerError $ "URI '" ++ urlStr ++ "' must be a github: flakeref, not '" ++ s ++ "'"
    Nothing -> readerError $ "couldn't not parse '" ++ urlStr ++ "' as an absolute URI"

argsParser :: Parser Args
argsParser =  Args
          <$> argument flakeRefReader
              ( metavar "FLAKE"
             <> help "the flake reference pointing to the repo to certify"
              )
          <*> strArgument
              ( metavar "DIR"
             <> help "the output directory for the flake (must not exist)"
              )
          <*> optional gitHubAccessTokenParser

argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
 <> header "generate-flake — Generate the flake to be built as part of certification"
  )

instrumentedMain :: EventBackend IO r MainSelector -> Args -> IO ()
instrumentedMain backend (Args {..}) = do
  withEvent backend CreateOutput \ev -> do
    addField ev output
    createDirectory output
  generateFlake (narrowEventBackend Generate backend) (const $ pure ()) githubToken flakeref output

main :: IO ()
main = do
  args <- execParser argsInfo
  backend <- simpleJsonStderrBackend renderMainSelector
  instrumentedMain backend args

data MainSelector f where
  CreateOutput :: MainSelector FilePath
  Generate :: forall f . GenerateFlakeSelector f -> MainSelector f

renderMainSelector :: RenderSelectorJSON MainSelector
renderMainSelector CreateOutput = ("create-output", \p -> ("path", toJSON p))
renderMainSelector (Generate s) = renderGenerateFlakeSelector s
