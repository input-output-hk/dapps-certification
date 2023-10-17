module IOHK.Certification.Persistence
  ( module X
  , MonadSelda
  , sqliteOpen
  , seldaClose
  , SeldaT
  , SQLite
  , Backend
  , SeldaConnection
  ) where

import Database.Selda

import Database.Selda.SQLite
import Database.Selda.Backend (SeldaConnection)

import           IOHK.Certification.Interface  as X
  ( GitHubAccessToken(..)
  , GitHubAccessTokenType(..)
  , ghAccessTokenPattern
  )

import IOHK.Certification.Persistence.Structure.Run as X
  ( Run(..)
  , Status(..)
  )
import IOHK.Certification.Persistence.Structure.Certification as X
  ( Certification(..)
  , L1Certification(..)
  , CertificationLevel(..)
  , L1CertificationDTO(..)
  )
import IOHK.Certification.Persistence.Migration as X
  ( ensureTables
  , MigrationSelector(..)
  , renderMigrationSelector
  )

import IOHK.Certification.Persistence.Structure as X
  ( DApp(..)
  , ProfileDTO(..)
  , DAppDTO(..)
  , IpfsCid(..)
  , TxId(..)
  , Transaction(..)
  , TxStatus(..)
  , TransactionEntry(..)
  , SubscriptionDTO(..)
  , TierDTO(..)
  , ProfileWallet(..)
  , WalletAddressStatus(..)
  , ProfileSummaryDTO(..)
  , RunStats(..)
  )
import Database.Selda as X
  ( fromId
  , toId
  )
import IOHK.Certification.Persistence.Structure.Profile as X
  ( ProfileId
  , Profile(..)
  , UserRole(..)
  )
import IOHK.Certification.Persistence.Pattern as X
import IOHK.Certification.Persistence.Structure.Subscription as X
  ( Subscription(..)
  , SubscriptionId
  , FeatureType(..)
  , TierId
  , TierType(..)
  , Tier(..)
  , SubscriptionLite(..)
  , SubscriptionStatus(..)
  )
import IOHK.Certification.Persistence.API as X
  ( MinimalTransaction(..)
  , MinimalTransactionEntry(..)
  , AdaUsdPrice
  , upsertProfile
  , upsertTransaction
  , getProfile
  , getProfileDApp
  , createRun
  , getRun
  , updateFinishedRun
  , getRuns
  , withConnection
  , getProfileId
  , getProfileAddress
  , syncRun
  , getRunOwner
  , getL1Certification
  , createL1Certificate
  , deleteRun
  , markAsAborted
  , getRunStatus
  , markAsReadyForCertification
  , getAllCertifiedRunsForAddress
  , getRunsToCertify
  , getProfileBalance
  , addInitialData
  , getProfileSubscriptions
  , getPendingSubscriptions
  , activateSubscription
  , activateAllPendingSubscriptions
  , createSubscription
  , cancelPendingSubscription
  , getAllTiers
  , getCurrentFeatures
  , getJWTSecret
  , insertJWTSecret
  , getAllTransactions
  , getProfileWallets
  , getProfileWallet
  , upsertProfileWallet
  , markAllRunningAsAborted
  , verifyImpersonation
  , Impersonation(..)
  , removeUserRole
  , removeAllUserRoles
  , hasAtLeastUserRole
  , hasSomeUserRoles
  , getUserRoles
  , addUserRole
  , updateUserRoles
  , ensureAdminExists
  , getAllProfilesByRole
  )
{-

Used in development to create the database schema,
in order to create migrations.
>>> import Database.Selda.SQLite (sqliteOpen, SQLite)
>>> import Database.Selda.Backend (runSeldaT)
>>> import IOHK.Certification.Persistence.Migration
>>> sqliteOpen "temp.sqlite" >>= runSeldaT createTables >> return "DONE"
"DONE"

-}
