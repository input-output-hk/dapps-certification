{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module Plutus.Certification.Persistence.Instrumentation where

import Observe.Event.Backend
import Control.Monad.Catch (MonadMask)
import IOHK.Certification.Persistence as DB
import Plutus.Certification.Internal
import Control.Monad.RWS
import Observe.Event
import Data.Maybe (isJust)
import Observe.Event.Render.JSON (RenderSelectorJSON)
import Data.UUID (UUID)
import Data.Time (UTCTime)
import Data.Set (Set)
import Data.Text (Text)
import Data.Aeson (ToJSON(toJSON))

data UpsertProfileFields
  = UpsertProfileFieldsAddress ProfileWalletAddress
  | UpsertProfileFieldsHasDApp Bool

data CreateSubscriptionFields
  = CreateSubscriptionFieldsPrice AdaUsdPrice
  | CreateSubscriptionFieldsProfileId (ID Profile)
  | CreateSubscriptionFieldsTierId (ID Tier)

data UpdateFinishedRunFields
  = UpdateFinishedRunFieldsRunId UUID
  | UpdateFinishedRunFieldsFinished Bool

data UpdateUserRolesFields
  = UpdateUserRolesFieldsProfileId (ID Profile)
  | UpdateUserRolesFieldsRoles (Set UserRole)

data MarkAsReadyForCertificationFields
  = MarkAsReadyForCertificationFieldsUUID UUID
  | MarkAsReadyForCertificationFieldsIpfsCid IpfsCid
  | MarkAsReadyForCertificationFieldsNow UTCTime

data CreateRunFields
  = CreateRunFieldsRunId UUID
  | CreateRunFieldsUrl Text
  | CreateRunFieldsCommitDate UTCTime
  | CreateRunFieldsCommitHash CommitHash
  | CreateRunFieldsCertificationPrice CertificationPrice
  | CreateRunFieldsProfileId (ID Profile)
  | CreateRunFieldsWithOptions Bool

data DbSelector f where
  UpsertProfile :: DbSelector UpsertProfileFields
  UpdateFinishedRun :: DbSelector UpdateFinishedRunFields
  DeleteRun :: DbSelector UUID
  MarkAsAborted :: DbSelector UUID
  CreateSubscription :: DbSelector CreateSubscriptionFields
  UpdateUserRoles :: DbSelector UpdateUserRolesFields
  MarkAsReadyForCertification :: DbSelector MarkAsReadyForCertificationFields
  CreateRun :: DbSelector CreateRunFields

upsertProfile :: (MonadReader env m,HasDb env, MonadIO m,MonadMask m)
              => EventBackend m r DbSelector
              -> Profile
              -> Maybe DApp
              -> m (Maybe (ID Profile))
upsertProfile eb p d = withEvent eb UpsertProfile $ \ev -> do
  addField ev $ UpsertProfileFieldsAddress (p.ownerAddress)
  addField ev $ UpsertProfileFieldsHasDApp (isJust d)
  withDb $ DB.upsertProfile p d

updateFinishedRun :: (MonadReader env m,HasDb env, MonadIO m,MonadMask m)
                  => EventBackend m r DbSelector
                  -> UUID
                  -> Bool
                  -> UTCTime
                  -> m Int
updateFinishedRun eb uuid finished now = withEvent eb UpdateFinishedRun $ \ev -> do
  addField ev $ UpdateFinishedRunFieldsRunId uuid
  addField ev $ UpdateFinishedRunFieldsFinished finished
  withDb $ DB.updateFinishedRun uuid finished now

deleteRun :: (MonadReader env m,HasDb env, MonadIO m,MonadMask m)
          => EventBackend m r DbSelector
          -> UUID
          -> m Int
deleteRun eb uuid = withEvent eb DeleteRun $ \ev -> do
  addField ev uuid
  withDb $ DB.deleteRun uuid

markAsAborted :: (MonadReader env m,HasDb env, MonadIO m,MonadMask m)
              => EventBackend m r DbSelector
              -> UUID
              -> UTCTime
              -> m Int
markAsAborted eb uuid now = withEvent eb MarkAsAborted $ \ev -> do
  addField ev uuid
  withDb $ DB.markAsAborted uuid now

createSubscription :: (MonadReader env m,HasDb env, MonadIO m,MonadMask m)
                   => EventBackend m r DbSelector
                   -> UTCTime
                   -> ID Profile
                   -> ID Tier
                   -> AdaUsdPrice
                   -> m (Maybe SubscriptionDTO)
createSubscription eb now profileId' tierId' adaUsdPrice' = withEvent eb CreateSubscription $ \ev -> do
  addField ev $ CreateSubscriptionFieldsPrice adaUsdPrice'
  addField ev $ CreateSubscriptionFieldsProfileId profileId'
  addField ev $ CreateSubscriptionFieldsTierId tierId'
  withDb $ DB.createSubscription now profileId' tierId' adaUsdPrice'

updateUserRoles :: (MonadReader env m,HasDb env, MonadIO m,MonadMask m)
                => EventBackend m r DbSelector
                -> ID Profile
                -> Set UserRole
                -> m Int
updateUserRoles eb profileId' roles = withEvent eb UpdateUserRoles $ \ev -> do
  addField ev $ UpdateUserRolesFieldsProfileId profileId'
  addField ev $ UpdateUserRolesFieldsRoles roles
  withDb $ DB.updateUserRoles profileId' roles

markAsReadyForCertification :: (MonadReader env m,HasDb env, MonadIO m,MonadMask m)
                            => EventBackend m r DbSelector
                            -> UUID
                            -> IpfsCid
                            -> UTCTime
                            -> m Int
markAsReadyForCertification eb uuid ipfsCid' now = withEvent eb MarkAsReadyForCertification $ \ev -> do
  addField ev $ MarkAsReadyForCertificationFieldsUUID uuid
  addField ev $ MarkAsReadyForCertificationFieldsIpfsCid ipfsCid'
  addField ev $ MarkAsReadyForCertificationFieldsNow now
  withDb $ DB.markAsReadyForCertification uuid ipfsCid' now

createRun :: (MonadReader env m,HasDb env, MonadIO m,MonadMask m)
          => EventBackend m r DbSelector
          -> UUID
          -> UTCTime
          -> Text
          -> UTCTime
          -> CommitHash
          -> CertificationPrice
          -> Bool
          -> ID Profile
          -> m ()
createRun eb uuid now uriTxt commitDate' commitHash' certificationPrice'
  withCustomOptions' profileId' = withEvent eb CreateRun $ \ev -> do

  addField ev $ CreateRunFieldsRunId uuid
  addField ev $ CreateRunFieldsUrl uriTxt
  addField ev $ CreateRunFieldsCommitDate commitDate'
  addField ev $ CreateRunFieldsCommitHash commitHash'
  addField ev $ CreateRunFieldsCertificationPrice certificationPrice'
  addField ev $ CreateRunFieldsProfileId profileId'
  addField ev $ CreateRunFieldsWithOptions withCustomOptions'

  withDb $ DB.createRun uuid now uriTxt commitDate' commitHash'
    certificationPrice' withCustomOptions' profileId'

renderPersistenceSelector :: RenderSelectorJSON DbSelector
renderPersistenceSelector UpsertProfile = ("upsert-profile",\case
  UpsertProfileFieldsAddress addr -> ("address",toJSON addr)
  UpsertProfileFieldsHasDApp hasDApp -> ("has-dapp",toJSON hasDApp)
  )
renderPersistenceSelector UpdateFinishedRun = ("update-finished-run",\case
  UpdateFinishedRunFieldsRunId uuid -> ("run-id",toJSON uuid)
  UpdateFinishedRunFieldsFinished finished -> ("finished",toJSON finished)
  )
renderPersistenceSelector DeleteRun = ("delete-run",\uuid -> ("run-id",toJSON uuid))
renderPersistenceSelector MarkAsAborted = ("mark-as-aborted",\uuid -> ("run-id",toJSON uuid))
renderPersistenceSelector CreateSubscription = ("create-subscription",\case
  CreateSubscriptionFieldsPrice adaUsdPrice' -> ("price",toJSON adaUsdPrice')
  CreateSubscriptionFieldsProfileId profileId' -> ("profile-id",toJSON profileId')
  CreateSubscriptionFieldsTierId tierId' -> ("tier-id",toJSON tierId')
  )
renderPersistenceSelector UpdateUserRoles = ("update-user-roles",\case
  UpdateUserRolesFieldsProfileId profileId' -> ("profile-id",toJSON profileId')
  UpdateUserRolesFieldsRoles roles -> ("roles",toJSON roles)
  )
renderPersistenceSelector MarkAsReadyForCertification = ("mark-as-ready-for-certification",\case
  MarkAsReadyForCertificationFieldsUUID uuid -> ("run-id",toJSON uuid)
  MarkAsReadyForCertificationFieldsIpfsCid ipfsCid' -> ("ipfs-cid",toJSON ipfsCid')
  MarkAsReadyForCertificationFieldsNow now -> ("now",toJSON now)
  )
renderPersistenceSelector CreateRun = ("create-run",\case
  CreateRunFieldsRunId uuid -> ("run-id",toJSON uuid)
  CreateRunFieldsUrl uriTxt -> ("url",toJSON uriTxt)
  CreateRunFieldsCommitDate commitDate' -> ("commit-date",toJSON commitDate')
  CreateRunFieldsCommitHash commitHash' -> ("commit-hash",toJSON commitHash')
  CreateRunFieldsCertificationPrice certificationPrice' -> ("certification-price",toJSON certificationPrice')
  CreateRunFieldsProfileId profileId' -> ("profile-id",toJSON profileId')
  )
