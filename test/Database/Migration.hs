{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}
module Database.Migration (spec) where

import Test.Hspec
import Database.Embedded
import System.IO.Temp
import System.IO
import Database.Selda.SQLite
import Database.Selda.Backend
import Observe.Event.Backend

import qualified Data.ByteString as BS
import qualified IOHK.Certification.Persistence as DB


migrateSpec :: BS.ByteString -> IO ()
migrateSpec bs = withSystemTempFile "certification.sqlite" \file h -> do
  BS.hPut h bs >> hClose h
  conn <- sqliteOpen file
  runSeldaT (DB.ensureTables unitEventBackend False) conn
  seldaClose conn

spec :: SpecWith ()
spec = describe "Migration" $ do
  it "start from empty db" $ do
      conn <- sqliteOpen @IO ":memory:"
      runSeldaT (DB.ensureTables unitEventBackend True) conn
      seldaClose conn

  it "migrates from v0" $ migrateSpec certificationV0
  it "migrates from v1" $ migrateSpec certificationV1
  it "migrates from v2" $ migrateSpec certificationV2
