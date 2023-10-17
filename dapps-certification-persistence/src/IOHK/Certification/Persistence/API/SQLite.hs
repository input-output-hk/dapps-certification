
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module IOHK.Certification.Persistence.API.SQLite
( getProfilesSummary
, sqlLiteGetAllTables
, withSQLiteConnection
) where

import Database.Selda hiding (Set)
import Database.Selda.Backend hiding (withConnection)
import Database.Selda.Unsafe
import IOHK.Certification.Persistence.Structure.Subscription
import IOHK.Certification.Persistence.Structure.Profile
import IOHK.Certification.Persistence.Structure
import IOHK.Certification.Persistence.FieldSelector
import Database.Selda.SQLite
import Text.RawString.QQ(r)
import Data.Data
import Data.FileEmbed
import Data.ByteString.Char8 (ByteString,unpack)
import Data.String (IsString(fromString))
-- | SQLite query to get all the profiles with their maximum role
-- | and their dapp if any

profileSummaryRawBS :: ByteString
profileSummaryRawBS = $(makeRelativeToProject
  "src/IOHK/Certification/Persistence/API/SQLite/profileSummary.sql"
  >>= embedFile
  )

profileSummaryRawQ :: QueryFragment
profileSummaryRawQ = fromString $ unpack profileSummaryRawBS

allTablesRawQ :: Text
allTablesRawQ =[r|
  SELECT name
  FROM sqlite_schema
  WHERE type IN ('table','view') AND name NOT LIKE 'sqlite_%'
  ORDER BY 1
|]

-- | Column names for the profile summary query
profileSummaryColumnNames :: [ColName]
profileSummaryColumnNames
   = fieldNames @Profile @ColName
  ++ ["profileId" , "role"]
  ++ fieldNames @DApp @ColName
  ++ fieldNames @RunStats @ColName
  ++ fieldNames @SubscriptionLite @ColName

-- >>> profileSummaryColumnNames
instance SqlRow ProfileSummaryDTO where

  nextResult = do
    profileResult <- nextResult
    let dappNextResult = do
          dapp <- nextResult
          if dappId dapp == toId (-1)
            then pure Nothing
            else pure $ Just $ DAppDTO dapp
    let statsNextResult = do
          stats <- nextResult
          if runsProfileId stats == toId (-1)
            then pure $ stats { runsProfileId = profileResult.profileId  }
            else pure stats
    ProfileSummaryDTO profileResult
        <$> (role <$> nextResult)
        <*> dappNextResult
        <*> statsNextResult                 -- @RunStats
        <*> nextResult                      -- @Subscription
  nestedCols _ = nestedCols (Proxy @Profile)
               + nestedCols (Proxy @ProfileRole)
               + nestedCols (Proxy @DApp)
               + nestedCols (Proxy @RunStats)
               + nestedCols (Proxy @SubscriptionLite)

getProfilesSummaryQ :: Query SQLite (Row SQLite ProfileSummaryDTO)
getProfilesSummaryQ = rawQuery profileSummaryColumnNames profileSummaryRawQ

-- >>> sqliteOpen "certification.sqlite" >>= runSeldaT (query getProfilesSummaryQ) -- >>= pure . (fmap summaryDapp)

getProfilesSummary :: (MonadSelda m, Backend m ~ SQLite) => m [ProfileSummaryDTO]
getProfilesSummary = query getProfilesSummaryQ

sqlLiteGetAllTables :: (MonadIO m,MonadMask m) => SeldaT SQLite m [Text]
sqlLiteGetAllTables = query sqlLiteGetAllTablesQ
  where
  sqlLiteGetAllTablesQ :: Query s (Col s Text)
  sqlLiteGetAllTablesQ = rawQuery1 "name" fragment

  fragment = inj col

  col :: Col s Text
  col = rawExp allTablesRawQ

withSQLiteConnection :: forall m a. (MonadIO m, MonadMask m) => SeldaConnection SQLite -> SeldaT SQLite m a -> m a
withSQLiteConnection = flip runSeldaT

-- >>> sqliteOpen "certification.sqlite" >>= runSeldaT sqlLiteGetAllTables
