{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IOHK.Certification.Persistence.Migration.SQLite.V1 where

import Text.RawString.QQ(r)
import Database.Selda.Unsafe (QueryFragment)

createTables :: [QueryFragment]
createTables =
  -- NOTE: lookupe table it might have been created by the previous migration
  -- or not, depending when the initial db was created
  -- so we need to make sure it exists
  [ [r|
    CREATE TABLE IF NOT EXISTS "lookup"(
      "lookupProp" TEXT  NOT NULL,
      "lookupValue" TEXT NOT NULL,
      PRIMARY KEY("lookupProp")
    ); |]

  ------------------------------------------------------------------------------
  -- | DApp table modification
  --
  -- Step 1: Create a new temporary table with the desired schema

  , [r|
      CREATE TABLE IF NOT EXISTS "dapp_temp" (
        "dappId" INTEGER NOT NULL UNIQUE,
        "dappName" TEXT NOT NULL,
        "dappOwner" TEXT NOT NULL,
        "dappRepo" TEXT NOT NULL,
        "dappVersion" TEXT,  -- Making it nullable
        "dappGitHubToken" TEXT NULL,
        "dappSubject" TEXT NULL,
        UNIQUE("dappId"),
        CONSTRAINT "fk0_dappId" FOREIGN KEY ("dappId") REFERENCES "profile"("profileId")
      ); |]
  -- Step 2: Copy data from the existing "dapp" table to the temporary table
  , [r|
      INSERT INTO "dapp_temp" ("dappId", "dappName", "dappOwner", "dappRepo", "dappVersion", "dappGitHubToken", "dappSubject")
      SELECT "dappId", "dappName", "dappOwner", "dappRepo", "dappVersion", "dappGitHubToken", "dappSubject"
      FROM "dapp";
    |]
  -- Step 3: Drop the existing "dapp" table
  , [r|
      DROP TABLE IF EXISTS "dapp";
    |]
  -- Step 4: Rename the temporary table to replace the original table
  , [r|
      ALTER TABLE "dapp_temp" RENAME TO "dapp";
    |]
  ------------------------------------------------------------------------------
  -- | Moving jwt-secret from dedicated table to lookup table
  , [r|
      INSERT OR IGNORE INTO "lookup" ("lookupProp", "lookupValue")
      SELECT 'jwt-secret', "Secret"
      FROM "jwt-secret";
    |]
  -- remove the jwt-secret table
  , [r| DROP TABLE IF EXISTS "jwt-secret"; |]

  -- Finally: upsert the db version (previous version was 0 but was not recorded in the beginning)
  -- so to be sure we don't deal with that we both insert (or ignore) and update
  , [r| INSERT OR IGNORE INTO "lookup" ("lookupProp", "lookupValue") VALUES ('version', '1'); |]
  , [r| UPDATE "lookup" SET "lookupValue" = '1' WHERE "lookupProp" = 'version'; |]
  ]

