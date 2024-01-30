{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module IOHK.Certification.Persistence.API.Profile where

import Control.Monad
import Data.Maybe
import Database.Selda hiding (Set)
import IOHK.Certification.Persistence.Structure.Profile
import IOHK.Certification.Persistence.Structure
import IOHK.Certification.Persistence.Pattern
import Data.Functor
import IOHK.Certification.Persistence.API.UserRole
import IOHK.Certification.Persistence.Structure.Certification

upsertProfile :: (MonadSelda m, MonadMask m) => Profile -> Maybe DApp -> m (Maybe (ID Profile))
upsertProfile profile@Profile{..} dappM = do
  void $ upsert profiles
     (\p -> p ! #ownerAddress .== literal ownerAddress)
     (`with`
      [ #website      := fromMaybe' website
      , #twitter      := fromMaybe' twitter
      , #linkedin     := fromMaybe' linkedin
      , #email        := fromMaybe' email
      , #contactEmail := fromMaybe' contactEmail
      , #companyName  := fromMaybe' companyName
      , #fullName     := fromMaybe' fullName
      ])
     [#profileId (def :: ID Profile) profile]
  -- we query this because upsert returns id only when inserts
  profileIdM <- getProfileId ownerAddress
  forM_ profileIdM $ \pid -> case dappM of
    -- if there is no dapp we delete the dapp entry
    Nothing -> do
        void $ deleteFrom dapps (\dapp -> dapp ! #dappId .== literal pid)
    -- if there is a dapp we upsert it
    Just dapp@DApp{..} -> do
      void $ upsert dapps
          (\dapp' -> dapp' ! #dappId .== literal pid)
          (`with`
           [ #dappName := text dappName
           , #dappOwner := text dappOwner
           , #dappRepo := text dappRepo
           , #dappVersion := literal dappVersion
           , #dappId := literal profileId
           , #dappGitHubToken := literal dappGitHubToken
           , #dappSubject := literal dappSubject
           ]
          )
          [dapp { dappId = pid }]
  pure profileIdM
  where
  fromMaybe' :: SqlType a => Maybe a -> Col s (Maybe a)
  fromMaybe' = maybe null_ (just . literal)

getProfileByAddress :: MonadSelda m => ProfileWalletAddress -> m (Maybe Profile)
getProfileByAddress address = listToMaybe <$> query (do
    p <- select profiles
    restrict (p ! #ownerAddress .== literal address)
    pure p
  )

getProfileQ :: ID Profile -> Query t (Row t Profile :*: Row t (Maybe DApp))
getProfileQ pid = do
  profile <- select profiles
  dapp <- leftJoin  (\dapp -> dapp ! #dappId .== literal pid) (select dapps)
  restrict (profile ! #profileId .== literal pid)
  pure (profile :*: dapp)

getProfileDAppQ :: ID Profile -> Query t (Row t DApp)
getProfileDAppQ pid = do
  dapp <- select dapps
  restrict (dapp ! #dappId .== literal pid)
  pure dapp

getProfileWalletQ :: ProfileId -> Query t (Row t Profile :*: Row t (Maybe ProfileWallet))
getProfileWalletQ profileId = do
  p <- select profiles
  restrict (p ! #profileId .== literal profileId)
  profileWallet <- leftJoin  (\pw -> pw ! #profileWalletId .== p ! #profileId) (select profileWallets)
  pure (p :*: profileWallet)

getProfileWallet :: MonadSelda f => ProfileId -> f (Maybe (Profile , Maybe ProfileWallet))
getProfileWallet profileId = fmap toTuple . listToMaybe <$> query (getProfileWalletQ profileId)

toTuple ::  a :*: b -> (a, b)
toTuple (p :*: pw) = (p,pw)

getProfileWalletsQ :: Query t (Row t Profile :*: Row t (Maybe ProfileWallet))
getProfileWalletsQ = do
  p <- select profiles
  profileWallet <- leftJoin  (\pw -> pw ! #profileWalletId .== p ! #profileId) (select profileWallets)
  pure (p :*: profileWallet)

getProfileWallets :: MonadSelda m => m [(Profile, Maybe ProfileWallet)]
getProfileWallets = map toTuple <$> query getProfileWalletsQ

upsertProfileWallet :: (MonadSelda m,MonadMask m) => ProfileWallet -> m ()
upsertProfileWallet ProfileWallet{..} = do
  void $ upsert profileWallets
    (\pw -> pw ! #profileWalletId .==  literal profileWalletId)
    (`with`
     [ #profileWalletAddress := text profileWalletAddress
     , #profileWalletStatus := literal profileWalletStatus
     , #profileWalletCredits := literal profileWalletCredits
     ])
    [ProfileWallet{..}]

getProfile' :: MonadSelda m => ID Profile -> m (Maybe Profile)
getProfile' pid = listToMaybe <$> query (do
  p <- select profiles
  restrict (p ! #profileId .== literal pid)
  pure p)


getProfile :: MonadSelda m => ID Profile -> m (Maybe ProfileDTO)
getProfile pid = do
   ret <- listToMaybe <$> query (getProfileQ pid)
   userRoles <- getUserRoles pid
   maxUserRole <- case userRoles of
      _ | null userRoles || userRoles == [NoRole] -> pure Nothing
      _ | userRole <- maximum userRoles -> pure $ Just userRole
   pure $ ret <&> \(r :*: dapp) ->
    ProfileDTO r (DAppDTO <$> dapp) maxUserRole

getProfileDApp :: MonadSelda m => ID Profile -> m (Maybe DApp)
getProfileDApp pid = fmap listToMaybe $ query $ getProfileDAppQ pid

getProfileIdQ:: ProfileWalletAddress -> Query t (Col t (ID Profile))
getProfileIdQ  address = do
  p <- select profiles
  restrict (p ! #ownerAddress .== literal address)
  pure (p ! #profileId)

getProfileId :: MonadSelda m => ProfileWalletAddress -> m (Maybe ProfileId)
getProfileId = fmap listToMaybe . query . getProfileIdQ

getProfileAddressQ :: ID Profile -> Query t (Col t ProfileWalletAddress)
getProfileAddressQ  pid = do
  p <- select profiles
  restrict (p ! #profileId .== literal pid)
  pure (p ! #ownerAddress)

getProfileAddress :: MonadSelda m => ID Profile -> m (Maybe ProfileWalletAddress)
getProfileAddress = fmap listToMaybe . query . getProfileAddressQ

ensureAdminExists :: (MonadSelda m) => Bool -> ProfileWalletAddress -> m Int
ensureAdminExists forceAdminAlways walletAddress = do
  -- first ensure there is at least one admin
  admins <- query $ do
    role <- select profileRoles
    restrict (role ! #role .== literal Admin)
    pure (role ! #profileId)
  -- get the profile
  profileM <- getProfileByAddress walletAddress
  case profileM of
    Just profile
      -- if there are no admins
      | null admins
      -- or when forceAdminAlways is True and the profile is not an admin
      || (forceAdminAlways && (profile.profileId `notElem` admins)) ->
         -- add the admin role to the profile and return the number of admins
         -- (including the new one)
         (+ length admins) <$> addUserRole (profile.profileId) Admin
    _ -> pure (length admins)

data Impersonation
  = ImpersonationNotAllowed
  | ImpersonationProfile (ID Profile, ProfileWalletAddress)
  | ImpersonationNotFound

-- verify minimum role and return the profileId
-- and the address of the impersonated user
verifyImpersonation :: MonadSelda m
                    => ID Profile
                    -> UserRole
                    -> ID Profile
                    -> m Impersonation
verifyImpersonation ownerPid role pid = do
  -- check if the impersonated user has at least the given role
  hasRole <- hasAtLeastUserRole ownerPid role
  if hasRole
  then do
    -- get the address of the impersonated user
    addressM <- getProfileAddress pid
    case addressM of
      Nothing -> pure ImpersonationNotFound
      Just address -> pure $ ImpersonationProfile (pid, address)
  else pure ImpersonationNotAllowed

getAllProfilesByRole :: MonadSelda m => UserRole -> m [ID Profile]
getAllProfilesByRole userRole = query $ do
  role <- select profileRoles
  restrict (role ! #role .== literal userRole)
  pure (role ! #profileId)

addAuditorReportEvent :: MonadSelda m => AuditorReportEvent -> m Int
addAuditorReportEvent report =
  insert auditorReportEvents [report { areId = def }]

getAuditorReportsInInterval :: MonadSelda m
                            => UTCTime
                            -> UTCTime
                            -> m [AuditorReportEvent]
getAuditorReportsInInterval from' to' = query $ do
  report <- select auditorReportEvents
  restrict (report ! #areCreatedAt .>= literal from'
            .&& report ! #areCreatedAt .< literal to')
  pure report


