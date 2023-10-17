{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IOHK.Certification.Persistence.Migration.SQLite.V2 where

import Text.RawString.QQ(r)
import Database.Selda.Unsafe (QueryFragment)

createTables :: [QueryFragment]
createTables =
  [
    -- Profile role table
    [r|
    CREATE TABLE IF NOT EXISTS "profile_role"(
      "profileId" INTEGER NOT NULL,
      "role" SMALLINT NOT NULL,
      CONSTRAINT "fk0_profileId" FOREIGN KEY ("profileId")
      REFERENCES "profile"( "profileId")
      UNIQUE ("profileId", "role")
    );
    |]
  , [r|
    CREATE INDEX IF NOT EXISTS "idx_profile_role_role" ON "profile_role"("role");
    |]
    -- increment version
  , [r| UPDATE "lookup" SET "lookupValue" = '2' WHERE "lookupProp" = 'version'; |]
  ]

