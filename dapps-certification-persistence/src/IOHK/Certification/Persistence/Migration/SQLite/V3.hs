{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IOHK.Certification.Persistence.Migration.SQLite.V3 where

import Text.RawString.QQ(r)
import Database.Selda.Unsafe (QueryFragment)

createTables :: [QueryFragment]
createTables =
  [ [r| 
      CREATE TABLE IF NOT EXISTS "auditor_report_events"(
        "areId" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        "areProfileId" INTEGER NOT NULL,
        "areCertLevel" BIGINT NOT NULL,
        "areCreatedAt" DATETIME NOT NULL,
        "areOffchainContentId" TEXT NOT NULL,
        CONSTRAINT "fk0_areProfileId" FOREIGN KEY ("areProfileId") REFERENCES "profile"("profileId")
     );
    |]
    -- increment version
  , [r| UPDATE "lookup" SET "lookupValue" = '3' WHERE "lookupProp" = 'version'; |]
  ]

