{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes          #-}

module IOHK.Certification.Persistence.Migration.SQLite.V0 where

import Text.RawString.QQ(r)
import Database.Selda.Unsafe (QueryFragment)

createTables :: [QueryFragment]
createTables =
  [ [r|
    CREATE TABLE IF NOT EXISTS "lookup"(
      "lookupProp" TEXT  NOT NULL,
      "lookupValue" TEXT NOT NULL,
      PRIMARY KEY("lookupProp")
    ); |]
  , [r|
      CREATE TABLE IF NOT EXISTS "certification"(
          "certId" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
          "certTransactionId" TEXT NOT NULL,
          "certCreatedAt" DATETIME NOT NULL
      ); |]
  , [r|
      CREATE TABLE IF NOT EXISTS "onchain_certifications" (
        "certId" INTEGER  NOT NULL,
        "profileId" INTEGER NOT NULL,
        "subject" TEXT NOT NULL,
        "rootHash" TEXT NOT NULL,
        "schemaVersion" TEXT NOT NULL,
        "certificationTypeId" INTEGER NOT NULL,
        PRIMARY KEY("certId"),
        CONSTRAINT "fk0_certId" FOREIGN KEY ("certId") REFERENCES "certification"("certId"),
        CONSTRAINT "fk1_profileId" FOREIGN KEY ("profileId") REFERENCES "profile"("profileId"),
        CONSTRAINT "fk2_certificationTypeId" FOREIGN KEY ("certificationTypeId") REFERENCES "certification_types"("typeId")
      ); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "profile" (
      "profileId" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      "ownerAddress" TEXT NOT NULL UNIQUE ,
      "website" TEXT NULL,
      "twitter" TEXT NULL,
      "linkedin" TEXT NULL,
      "email" TEXT NULL,
      "contactEmail" TEXT NULL,
      "companyName" TEXT NULL,
      "fullName" TEXT NULL,
      UNIQUE("ownerAddress")
      ); |]
  , [r| CREATE INDEX ixprofile_ownerAddress ON "profile" ("ownerAddress"); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "profile_wallet" (
      "profileWalletId" INTEGER  NOT NULL,
      "profileWalletAddress" TEXT NOT NULL,
      "profileWalletStatus" BIGINT NOT NULL,
      "profileWalletCredits" BIGINT NOT NULL,
      PRIMARY KEY("profileWalletId"),
      CONSTRAINT "fk0_profileWalletId" FOREIGN KEY ("profileWalletId") REFERENCES "profile"("profileId")
      ); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "dapp" (
      "dappId" INTEGER NOT NULL UNIQUE,
      "dappName" TEXT NOT NULL,
      "dappOwner" TEXT NOT NULL,
      "dappRepo" TEXT NOT NULL,
      "dappVersion" TEXT NOT NULL,
      "dappGitHubToken" TEXT NULL,
      "dappSubject" TEXT NULL,
      UNIQUE("dappId"),
      CONSTRAINT "fk0_dappId" FOREIGN KEY ("dappId") REFERENCES "profile"("profileId")
      ); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "run" (
      "runId" BLOB  NOT NULL,
      "created" DATETIME NOT NULL ,
      "finishedAt" DATETIME NULL,
      "syncedAt" DATETIME NOT NULL,
      "repoUrl" TEXT NOT NULL,
      "commitDate" DATETIME NOT NULL,
      "commitHash" TEXT NOT NULL,
      "runStatus" TEXT NOT NULL,
      "profileId" INTEGER NOT NULL,
      "certificationPrice" BIGINT NOT NULL,
      "reportContentId" TEXT NULL,
      PRIMARY KEY("runId"),
      CONSTRAINT "fk0_profileId" FOREIGN KEY ("profileId") REFERENCES "profile"("profileId")
      ); |]
  , [r| CREATE INDEX ixrun_created ON "run" ("created"); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "transaction" (
      "wtxId" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      "wtxExternalId" TEXT NOT NULL UNIQUE,
      "wtxAmount" BIGINT NOT NULL,
      "wtxTime" DATETIME NOT NULL,
      "wtxDepth" BIGINT NOT NULL,
      "wtxStatus" TEXT NOT NULL,
      "wtxMetadata" TEXT NOT NULL,
      UNIQUE("wtxExternalId")
    ); |]
  , [r| CREATE INDEX ixtransaction_wtxTime ON "transaction" ("wtxTime"); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "transaction_entry"(
      "txEntryId" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      "txEntryIndex" BIGINT NULL,
      "txEntryAddress" TEXT NOT NULL,
      "txEntryAmount" BIGINT NOT NULL,
      "txEntryInput" BOOLEAN NOT NULL,
      "txEntryTxId" INTEGER NOT NULL,
      CONSTRAINT "fk0_txEntryTxId" FOREIGN KEY ("txEntryTxId") REFERENCES "transaction"("wtxId")
      ); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "feature"(
      "Id" TEXT  NOT NULL,
      "Name" TEXT NOT NULL UNIQUE,
      UNIQUE("Name"),
      PRIMARY KEY("Id")
      ); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "tier"(
      "Id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      "Name" TEXT NOT NULL,
      "Subtitle" TEXT NOT NULL,
      "Description" TEXT NOT NULL,
      "Type" TEXT NOT NULL,
      "UsdPrice" DOUBLE PRECISION NOT NULL,
      "Duration" BIGINT NOT NULL,
      "Enabled" BOOLEAN NOT NULL
      ); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "tier_feature"(
      "Id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      "TierId" INTEGER NOT NULL,
      "FeatureId" TEXT NOT NULL,
      CONSTRAINT "fk0_TierId" FOREIGN KEY ("TierId") REFERENCES "tier"("Id"),
      CONSTRAINT "fk1_FeatureId" FOREIGN KEY ("FeatureId") REFERENCES "feature"("Id")
      ); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "subscription"(
      "Id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
      "ProfileId" INTEGER NOT NULL,
      "TierId" INTEGER NOT NULL,
      "Name" TEXT NOT NULL,
      "Type" TEXT NOT NULL,
      "Price" BIGINT NOT NULL,
      "AdaUsdPrice" DOUBLE PRECISION NOT NULL,
      "StartDate" DATETIME NOT NULL,
      "EndDate" DATETIME NOT NULL,
      "Status" BIGINT NOT NULL,
      CONSTRAINT "fk0_ProfileId" FOREIGN KEY ("ProfileId") REFERENCES "profile"("profileId"),
      CONSTRAINT "fk1_TierId" FOREIGN KEY ("TierId") REFERENCES "tier"("Id")
      ); |]
  , [r|
    CREATE TABLE IF NOT EXISTS "l1Certification"(
      "l1CertRunId" BLOB  NOT NULL,
      "l1CertId" INTEGER NOT NULL UNIQUE,
      UNIQUE("l1CertId"),
      PRIMARY KEY("l1CertRunId"),
      CONSTRAINT "fk0_l1CertRunId" FOREIGN KEY ("l1CertRunId") REFERENCES "run"("runId"),
      CONSTRAINT "fk1_l1CertId" FOREIGN KEY ("l1CertId") REFERENCES "certification"("certId")
      ); |]
  , [r| CREATE TABLE IF NOT EXISTS "jwt-secret"("Secret" TEXT NOT NULL); |]

  -- set the db version
  , [r| INSERT OR IGNORE INTO "lookup" ("lookupProp", "lookupValue") VALUES ('version', '0'); |]
  ]

