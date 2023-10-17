{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Plutus.Certification.Server.Internal where

import Conduit
import Control.Monad.Catch hiding (Handler)
import Data.ByteString.Char8 as BS hiding (hPutStrLn,foldl')
import Data.Aeson
import Data.Void
import Servant
import Control.Monad.State.Strict
import Observe.Event.BackendModification
import Observe.Event.Render.JSON
import Network.URI
import IOHK.Certification.Interface hiding (Status)
import Servant.Server.Experimental.Auth
import Plutus.Certification.API as API
import Data.Time
import Data.Text as Text hiding (elem,replicate, last)
import Data.Text.Encoding
import Data.UUID
import Control.Monad.Except
import Plutus.Certification.WalletClient (WalletAddress)
import Plutus.Certification.Internal

import qualified IOHK.Certification.Persistence as DB
import Control.Monad.RWS (MonadReader)

-- | Capabilities needed to run a server for 'API'
data ServerCaps m r = ServerCaps
  { -- | Submit a new certification job
    submitJob :: !(EventBackendModifiers r r -> Maybe GitHubAccessToken -> FlakeRefV1 -> m RunIDV1)
  , -- | Get the status of all runs associated with a job
    getRuns :: !(EventBackendModifiers r r -> RunIDV1 -> ConduitT () RunStatusV1 m ())
  , -- | Delete all runs associated with a job
    abortRuns :: !(EventBackendModifiers r r -> RunIDV1 -> m ())
  , -- | Get the logs for all runs associated with a job
    getLogs :: !(EventBackendModifiers r r -> Maybe KnownActionType -> RunIDV1 -> ConduitT () RunLog m ())
  }

data CreateRunField
  = CreateRunRef !FlakeRefV1
  | CreateRunID !RunIDV1

data CreateL1CertificationField
  = CreateL1CertificationRunID !RunIDV1
  | CreateL1CertificationReportIpfsCid !DB.IpfsCid
  | CreateL1CertificationMetadataIpfsCid !DB.IpfsCid
  | CreateL1CertificationDryRun !Bool

data GetRepoInfoField
  = GetRepoInfoOwner !Text
  | GetRepoInfoRepo !Text

type Error = String
newtype GenerateGitHubTokenField = GenerateGitHubTokenError Error

data SubscribeField
  = SubscribeFieldTierId !DB.TierId
  | SubscribeFieldProfileId !DB.ProfileId
  | SubscribeFieldSubscriptionId !DB.SubscriptionId

data CancelProfilePendingSubscriptionsField
  = CancelProfilePendingSubscriptionsFieldProfileId !DB.ProfileId
  | CancelProfilePendingSubscriptionsFieldCount !Int

data GetActiveFeaturesField
  = GetActiveFeaturesFieldProfileId !DB.ProfileId
  | GetActiveFeaturesFieldFeatures ![DB.FeatureType]

data CreateAuditorReportField
  = CreateAuditorReportFieldProfileId !DB.ProfileId
  -- is dry run
  | CreateAuditorReportDryRun !Bool
  -- ifpfs cid
  | CreateAuditorReportIpfsCid !DB.IpfsCid

data UpdateProfileRolesField
  = UpdateProfileRolesFieldProfileId DB.ProfileId
  | UpdateProfileRolesFieldRoles [DB.UserRole]

data ServerEventSelector f where
  Version :: ServerEventSelector Void
  WalletAddress :: ServerEventSelector Void
  CreateRun :: ServerEventSelector CreateRunField
  GetRun :: ServerEventSelector Void
  AbortRun :: ServerEventSelector RunIDV1
  GetRunDetails :: ServerEventSelector RunIDV1
  GetRunLogs :: ServerEventSelector RunIDV1
  GetProfileBalance :: ServerEventSelector DB.ProfileId
  GetCertification :: ServerEventSelector RunIDV1
  GetRepoInfo :: ServerEventSelector GetRepoInfoField
  CreateL1Certification :: ServerEventSelector CreateL1CertificationField
  Login :: ServerEventSelector WalletAddress
  ServerTimestamp :: ServerEventSelector Void
  GenerateGitHubToken :: ServerEventSelector GenerateGitHubTokenField
  GetGitHubClientId :: ServerEventSelector Void
  GetProfileSubscriptions :: ServerEventSelector DB.ProfileId
  Subscribe :: ServerEventSelector SubscribeField
  CancelProfilePendingSubscriptions :: ServerEventSelector CancelProfilePendingSubscriptionsField
  GetAllTiers :: ServerEventSelector Int
  GetActiveFeatures :: ServerEventSelector GetActiveFeaturesField
  GetAdaUsdPrice :: ServerEventSelector DB.AdaUsdPrice
  CreateAuditorReport :: ServerEventSelector CreateAuditorReportField
  GetProfileWalletAddress :: ServerEventSelector DB.ProfileId
  UpdateProfile :: ServerEventSelector DB.ProfileId
  UpdateProfileRoles :: ServerEventSelector UpdateProfileRolesField
  GetProfileRoles :: ServerEventSelector DB.ProfileId
  GetAllProfilesByRole :: ServerEventSelector DB.UserRole
  GetProfilesSummary :: ServerEventSelector DB.ProfileId
  InternalError :: forall a. ToJSON a => ServerEventSelector a

renderServerEventSelector :: RenderSelectorJSON ServerEventSelector
renderServerEventSelector Version = ("version", absurd)
renderServerEventSelector WalletAddress = ("wallet-address", absurd)
renderServerEventSelector GetRun = ("get-run", absurd)
renderServerEventSelector AbortRun = ("abort-run", renderRunIDV1)
renderServerEventSelector GetRunLogs = ("get-run-logs", renderRunIDV1)
renderServerEventSelector GetRunDetails = ("get-run-details", renderRunIDV1)
renderServerEventSelector GetProfileBalance = ("get-profile-balance", renderProfileId)
renderServerEventSelector GetCertification = ("get-certification", renderRunIDV1)
renderServerEventSelector Login = ("login", \address' -> ("user-address", toJSON address'))
renderServerEventSelector ServerTimestamp = ("server-timestamp", absurd)
renderServerEventSelector GenerateGitHubToken = ("generate-github-token", \
    (GenerateGitHubTokenError err) -> ("error", toJSON err)
  )
renderServerEventSelector GetGitHubClientId = ("get-github-client-id", absurd)
renderServerEventSelector GetProfileWalletAddress = ("get-profile-wallet-address", renderProfileId)

renderServerEventSelector GetProfileSubscriptions = ("get-profile-subscriptions", renderProfileId)
renderServerEventSelector Subscribe = ("subscribe", \case
    SubscribeFieldTierId tid -> ("tier-id", toJSON tid)
    SubscribeFieldProfileId pid -> ("profile-id", toJSON (show pid))
    SubscribeFieldSubscriptionId sid -> ("subscription-id", toJSON (show sid))
  )
renderServerEventSelector CancelProfilePendingSubscriptions = ("cancel-profile-pending-subscriptions", \case
    CancelProfilePendingSubscriptionsFieldProfileId pid -> ("profile-id", toJSON (show pid))
    CancelProfilePendingSubscriptionsFieldCount count' -> ("deleted-count", toJSON count')
  )
renderServerEventSelector CreateRun = ("create-run", \case
    CreateRunRef fr -> ("flake-reference", toJSON $ uriToString id fr.uri "")
    CreateRunID rid -> ("run-id", toJSON rid)
  )

renderServerEventSelector CreateL1Certification = ("start-certification", \case
    CreateL1CertificationRunID rid -> ("run-id", toJSON rid)
    CreateL1CertificationReportIpfsCid cid -> ("report-ipfs-cid", toJSON cid)
    CreateL1CertificationMetadataIpfsCid cid -> ("metadata-ipfs-cid", toJSON cid)
    CreateL1CertificationDryRun dryRun -> ("dry-run", toJSON dryRun)
  )

renderServerEventSelector GetRepoInfo = ("get-repo-info", \case
    GetRepoInfoOwner owner -> ("owner", toJSON owner)
    GetRepoInfoRepo repo -> ("repo", toJSON repo)
  )
renderServerEventSelector GetAllTiers = ("get-all-tiers", \count' -> ("count", toJSON count'))
renderServerEventSelector GetActiveFeatures = ("get-active-features", \case
    GetActiveFeaturesFieldProfileId pid -> ("profile-id", toJSON (show pid))
    GetActiveFeaturesFieldFeatures features -> ("features", toJSON features)
  )
renderServerEventSelector GetAdaUsdPrice = ("get-ada-usd-price", \price -> ("price", toJSON price))
renderServerEventSelector CreateAuditorReport = ("create-auditor-report", \case
    CreateAuditorReportFieldProfileId pid -> ("profile-id", toJSON (show pid))
    CreateAuditorReportDryRun isDryRun -> ("is-dry-run", toJSON isDryRun)
    CreateAuditorReportIpfsCid  cid -> ("cid", toJSON cid)
  )
renderServerEventSelector InternalError =
  ("internal-error", ("something-went-wrong",) . toJSON )
renderServerEventSelector UpdateProfile = ("update-profile", renderProfileId)
renderServerEventSelector UpdateProfileRoles = ("update-profile-roles", \case
    UpdateProfileRolesFieldProfileId pid -> ("profile-id", toJSON (show pid))
    UpdateProfileRolesFieldRoles roles -> ("roles", toJSON roles)
  )
renderServerEventSelector GetProfileRoles = ("get-profile-roles", renderProfileId)
renderServerEventSelector GetAllProfilesByRole = ("get-all-profiles-by-role",
  \role' -> ("role", toJSON role'))
renderServerEventSelector GetProfilesSummary = ("get-profiles-summary", renderProfileId)


renderRunIDV1 :: RenderFieldJSON RunIDV1
renderRunIDV1 rid = ("run-id",toJSON rid)

renderProfileId :: RenderFieldJSON DB.ProfileId
renderProfileId pid = ("profile-id",toJSON (show pid))

type instance AuthServerData (AuthProtect "public-key") = (DB.ProfileId,ProfileWalletAddress)
type instance AuthServerData (AuthProtect "jwt-token") = (DB.ProfileId,ProfileWalletAddress)

toDbStatus :: RunStatusV1 -> DB.Status
toDbStatus (Finished _)= DB.Succeeded
toDbStatus (Incomplete (Preparing Failed)) = DB.Failed
toDbStatus (Incomplete (Building Failed)) = DB.Failed
toDbStatus (Incomplete (Certifying (CertifyingStatus Failed _ _))) = DB.Failed
toDbStatus _ = DB.Queued

toCertificationResult :: RunStatusV1 -> Maybe CertificationResult
toCertificationResult (Finished rep)= Just rep
toCertificationResult _ = Nothing

dbSync :: (MonadIO m,MonadMask m,MonadReader env m, HasDb env)
       => UUID -> RunStatusV1 -> m RunStatusV1
dbSync uuid' status = do
  now <- getNow
  let dbStatus = toDbStatus status
  void $ withDb $ case dbStatus of
    DB.Queued -> DB.syncRun uuid' now
    -- this will change to failed or succeeded only
    -- if the status is == Queued
    _ -> DB.updateFinishedRun uuid' (dbStatus == DB.Succeeded) now
  return status

getNow :: MonadIO m => m UTCTime
getNow = liftIO getCurrentTime

consumeRuns :: Monad m => ConduitT RunStatusV1 o (StateT IncompleteRunStatus m) RunStatusV1
consumeRuns = await >>= \case
  Nothing -> Incomplete <$> get
  Just (Incomplete s) -> do
    modify \s' -> case (s, s') of
      (_, Queued) -> s
      (Queued, _) -> s'

      (Preparing st, Preparing st') -> case compare st st' of
        LT -> s'
        _ -> s
      (_, Preparing _) -> s
      (Preparing _, _) -> s'

      (Building st, Building st') -> case compare st st' of
        LT -> s'
        _ -> s
      (_, Building _) -> s
      (Building _, _) -> s'

      (Certifying (CertifyingStatus st mp _), Certifying (CertifyingStatus st' mp' _)) -> case compare st st' of
        LT -> s'
        GT -> s
        EQ -> case (mp, mp') of
          (_, Nothing) -> s
          (Nothing, _) -> s'
          (Just p, Just p') -> case compare p.progressIndex p'.progressIndex of
            LT -> s'
            _ -> s
    consumeRuns
  Just s -> pure s

-- | this will create a new profile if there isn't any with the given address
ensureProfile :: forall m env. ( MonadIO m , MonadError ServerError m, MonadMask m
                               , MonadReader env m, HasDb env
                               )
              => ByteString -> m (DB.ProfileId,ProfileWalletAddress)
ensureProfile bs = do
  case DB.mkPatternedText (decodeUtf8 bs) of
    Left err -> throwError $ err400 { errBody = BS.fromStrict $ BS.pack err }
    Right address' -> do
      profileIdM <- withDb $ DB.getProfileId address'
      ensureProfile' profileIdM address'
  where
  ensureProfile'  (Just pid) address' = pure (pid, address')
  ensureProfile'  Nothing address' = do
    pidM <- withDb $ DB.upsertProfile
      (DB.Profile undefined address' Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
      Nothing
    case pidM of
      Nothing -> throwError $ err500 { errBody = "Profile couldn't be created" }
      Just pid -> pure (pid, address')
