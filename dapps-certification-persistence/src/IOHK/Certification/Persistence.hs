module IOHK.Certification.Persistence
  ( module X
  , MonadSelda
  ) where
import Database.Selda

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
import IOHK.Certification.Persistence.Structure as X
  ( DApp(..)
  , ProfileDTO(..)
  , DAppDTO(..)
  , createTables
  , IpfsCid(..)
  , TxId(..)
  , Transaction(..)
  , TxStatus(..)
  , TransactionEntry(..)
  , SubscriptionDTO(..)
  , TierDTO(..)
  , ProfileWallet(..)
  , WalletAddressStatus(..)
  )
import Database.Selda as X
  ( fromId
  , toId
  )
import IOHK.Certification.Persistence.Structure.Profile as X
  ( ProfileId
  , Profile(..)
  )
import IOHK.Certification.Persistence.Pattern as X
import IOHK.Certification.Persistence.Structure.Subscription as X
  ( Subscription(..)
  , SubscriptionId
  , FeatureType(..)
  , TierId
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
  , withSQLite'
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
  )
