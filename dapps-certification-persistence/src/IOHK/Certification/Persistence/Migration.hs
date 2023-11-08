{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}

module IOHK.Certification.Persistence.Migration where

import           Database.Selda
import           IOHK.Certification.Persistence.Structure.Profile
import           IOHK.Certification.Persistence.Structure.Subscription
import           IOHK.Certification.Persistence.Structure.Certification
import           IOHK.Certification.Persistence.Structure.Run
import           Data.Text hiding (index)
import           IOHK.Certification.Persistence.Structure
import           IOHK.Certification.Persistence.API
import           Database.Selda.Unsafe (rawStm)
import           Control.Monad.Catch
import           Observe.Event
import           Observe.Event.Render.JSON

import qualified IOHK.Certification.Persistence.Migration.SQLite.V0 as V0
import qualified IOHK.Certification.Persistence.Migration.SQLite.V1 as V1
import qualified IOHK.Certification.Persistence.Migration.SQLite.V2 as V2
import qualified IOHK.Certification.Persistence.Migration.SQLite.V3 as V3
import Control.Monad (forM_)
import Data.Aeson (ToJSON(toJSON))

-- | NOTE: this remains here for inspecting the current schema
-- to be used into debugging
createTables :: (MonadSelda m,MonadMask m) => m ()
createTables = do
  createTable certifications
  createTable onChainCertifications
  createTable profiles
  createTable profileWallets
  createTable dapps
  createTable profileRoles
  createTable runs
  createTable transactions
  createTable transactionEntries
  createTable features
  createTable tiers
  createTable tierFeatures
  createTable subscriptions
  createTable l1Certifications
  createTable lookupValues
  -- v3
  createTable auditorReportEvents


--------------------------------------------------------------------------------
-- | MIGRATION

createTablesV0 :: (MonadSelda m,MonadMask m) => m ()
createTablesV0 = do
  mapM_ rawStm V0.createTables

createTablesV1 :: (MonadSelda m,MonadMask m) => m ()
createTablesV1 = do
  mapM_ rawStm V1.createTables

createTablesV2 :: (MonadSelda m,MonadMask m) => m ()
createTablesV2 = do
  mapM_ rawStm V2.createTables

createTablesV3 :: (MonadSelda m,MonadMask m) => m ()
createTablesV3 = do
  mapM_ rawStm V3.createTables

steps :: (MonadSelda m,MonadMask m) => [m ()]
steps = [createTablesV0,createTablesV1,createTablesV2,createTablesV3]

getDBVersion :: (MonadSelda m,MonadMask m) => m Int
getDBVersion = do
  versionM <- getLookupValue "version"
  case versionM of
    Nothing -> return 0
    Just vText -> pure $ read (unpack vText)

ensureTables :: (MonadSelda m,MonadMask m)
             => EventBackend m r MigrationSelector
             -> Bool
             -> m ()
ensureTables eb isEmpty = withEvent eb EnsureTables $ \ev -> do
  -- get the current version
  version' <- catchAll getDBVersion (const $ return 0)
  let stepsIx = Prelude.zip [0..] steps

  -- add the db version and the current version to the event
  addField ev (DBVersion version')
  addField ev (AppVersion (Prelude.length stepsIx - 1))

  -- get the steps necessary to migrate to the latest version
  let remainingSteps = Prelude.drop (if isEmpty then 0 else version'+1) stepsIx

  -- execute the remaining steps
  transaction $ forM_ remainingSteps (executeStep ev)
  where
  executeStep ev (version,work) = withSubEvent ev MigrateDbTo $ \ev' ->
    addField ev' (MigrationStep version) >> work

--------------------------------------------------------------------------------
-- | INSTRUMENTATION FOR MIGRATION

renderMigrationSelector :: RenderSelectorJSON MigrationSelector
renderMigrationSelector EnsureTables = ("ensure-db-schema",\case
  AppVersion v -> ("app-orm-version",toJSON v)
  DBVersion v -> ("db-version",toJSON v)
  )

renderMigrationSelector MigrateDbTo = ("migrate-db-to",\
  (MigrationStep x) -> ("migration-step",toJSON x)
  )

data EnsureTablesFields =  AppVersion Int | DBVersion Int

newtype MigrateDbToFields =  MigrationStep Int
data MigrationSelector f where
  EnsureTables :: MigrationSelector EnsureTablesFields
  MigrateDbTo :: MigrationSelector MigrateDbToFields
