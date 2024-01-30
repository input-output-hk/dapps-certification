{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IOHK.Certification.Persistence.Migration.SQLite.V5 where

import Text.RawString.QQ(r)
import Database.Selda.Unsafe (QueryFragment)

createTables :: [QueryFragment]
createTables =
  [
    -- invoice table
    [r|
      CREATE TABLE IF NOT EXISTS "invoice"(
        "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        "date" DATETIME NOT NULL ,
        "profileId" INTEGER NOT NULL,
        "ownerAddress" TEXT NOT NULL,
        "website" TEXT NULL,
        "email" TEXT NULL,
        "companyName" TEXT NOT NULL,
        "fullName" TEXT NOT NULL,
        "adaUsdPrice" DOUBLE PRECISION NOT NULL,
        "cancelledInvId" INTEGER NULL,
        CONSTRAINT "fk0_profileId" FOREIGN KEY ("profileId") REFERENCES "profile"("profileId"),
        CONSTRAINT "fk1_cancelledInvId" FOREIGN KEY ("cancelledInvId") REFERENCES "invoice"("id")
      );
    |]
  , [r|
      CREATE INDEX ixinvoice_date ON "invoice" ("date");
    |]
    -- invoice item table
  , [r|
      CREATE TABLE IF NOT EXISTS "invoice_item"(
        "invId" INTEGER NOT NULL,
        "ix" BIGINT NOT NULL ,
        "name" TEXT NOT NULL,
        "count" BIGINT NOT NULL,
        "unitPrice" BIGINT NOT NULL,
        "vatPercentage" BIGINT NOT NULL,
        CONSTRAINT "fk0_invId" FOREIGN KEY ("invId") REFERENCES "invoice"("id"),
        PRIMARY KEY (invId, ix)
      );
    |]
    -- subscription invoice table
  , [r|
      CREATE TABLE IF NOT EXISTS "subscription_invoice"(
        "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
        "invId" INTEGER NOT NULL,
        "subId" INTEGER NOT NULL,
        CONSTRAINT "fk0_invId" FOREIGN KEY ("invId") REFERENCES "invoice"("id"),
        CONSTRAINT "fk1_subId" FOREIGN KEY ("subId") REFERENCES "subscription"("Id")
      );
    |]
  , [r|
      CREATE INDEX ixsubscription_invoice_subId ON "subscription_invoice" ("subId");
    |]
    -- increment version
  , [r| UPDATE "lookup" SET "lookupValue" = '5' WHERE "lookupProp" = 'version'; |]
  ]
