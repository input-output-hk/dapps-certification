{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import IOHK.Certification.Actions
import Observe.Event
import Observe.Event.Render.JSON
import Observe.Event.Render.IO.JSON
import IOHK.Certification.Interface

data Args = Args
  { flake :: !FilePath
  , githubToken :: !(Maybe GitHubAccessToken)
  }


argsParser :: Parser Args
argsParser =  Args
          <$> strArgument
              ( metavar "FLAKE"
             <> help "the path to the flake"
              )
          <*> optional gitHubAccessTokenParser

argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
 <> header "build-flake — Build the flake to be run as part of certification"
  )

instrumentedMain :: EventBackend IO r MainSelector -> Args -> IO ()
instrumentedMain backend (Args {..}) = do
  res <- buildFlake (narrowEventBackend Build backend) (const $ pure ()) githubToken flake
  putStrLn res

main :: IO ()
main = do
  args <- execParser argsInfo
  backend <- simpleJsonStderrBackend renderMainSelector
  instrumentedMain backend args

data MainSelector f where
  Build :: forall f . BuildFlakeSelector f -> MainSelector f

renderMainSelector :: RenderSelectorJSON MainSelector
renderMainSelector (Build s) = renderBuildFlakeSelector s
