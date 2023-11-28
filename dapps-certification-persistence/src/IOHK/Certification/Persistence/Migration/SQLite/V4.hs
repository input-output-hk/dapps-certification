{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IOHK.Certification.Persistence.Migration.SQLite.V4 where

import Text.RawString.QQ(r)
import Database.Selda.Unsafe (QueryFragment)

createTables :: [QueryFragment]
createTables =
  [ [r| 
      ALTER TABLE "run"
      ADD COLUMN "withCustomOptions" BOOLEAN NOT NULL DEFAULT 0;
    |]
    -- this is a fix for auditor report index from V3
  , [r|
    CREATE INDEX ixauditor_report_events_areCreatedAt ON "auditor_report_events" ("areCreatedAt");
    |]
    -- increment version
  , [r| UPDATE "lookup" SET "lookupValue" = '4' WHERE "lookupProp" = 'version'; |]
  ]
