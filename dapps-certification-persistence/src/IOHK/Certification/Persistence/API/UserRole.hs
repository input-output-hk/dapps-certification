{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}

module IOHK.Certification.Persistence.API.UserRole where

import Database.Selda hiding (Set)
import IOHK.Certification.Persistence.Structure.Profile
import Data.Set (Set,toList)

-- | Add a role to a profile
addUserRole :: MonadSelda m => ID Profile -> UserRole -> m Int
addUserRole pid role' = insert profileRoles [ProfileRole pid role']

updateUserRoles :: MonadSelda m => ID Profile -> Set UserRole -> m Int
updateUserRoles pid roles = do
  -- remove all the roles
  _ <- removeAllUserRoles pid
  -- add the new roles
  insert profileRoles (map (ProfileRole pid) (toList  roles))

-- | Get all the roles for a profile
getUserRoles :: MonadSelda m => ID Profile -> m [UserRole]
getUserRoles pid = query $ do
  role' <- select profileRoles
  restrict (role' ! #profileId .== literal pid)
  pure (role' ! #role)

-- | Check if a profile has at least one of the given roles
hasSomeUserRoles :: MonadSelda m => ID Profile -> [UserRole] -> m Bool
hasSomeUserRoles pid roles = do
  userRoles <- getUserRoles pid
  pure $ any (`elem` roles) userRoles

-- | Check if a profile has at least a given role level
-- e.g.
--
-- 1. Profile (Support)
-- hasAtLeastUserRole pid Support == True
-- hasAtLeastUserRole pid Admin == False
--
-- 2. Profile (Admin)
-- hasAtLeastUserRole pid Support == True
-- hasAtLeastUserRole pid Admin == True
--
hasAtLeastUserRole :: MonadSelda m => ID Profile -> UserRole -> m Bool
hasAtLeastUserRole pid role' = do
  roles <- query $ hasAtLeastUserRole' pid role'
  pure $ not $ null roles

hasAtLeastUserRole' :: ID Profile -> UserRole -> Query t (Col t UserRole)
hasAtLeastUserRole' pid providedRole = do
  role' <- select profileRoles
  restrict (role' ! #profileId .== literal pid
       .&& role' ! #role .>= literal providedRole)
  pure (role' ! #role)

-- | Remove all the roles for a profile
-- returns the number of deleted roles
removeAllUserRoles :: MonadSelda m => ID Profile -> m Int
removeAllUserRoles pid =
  deleteFrom profileRoles (\role' -> role' ! #profileId .== literal pid)

-- | Remove a role for a profile
-- returns True if the role was removed
-- returns False if the role was not found
removeUserRole :: MonadSelda m => ID Profile -> UserRole -> m Bool
removeUserRole pid roleToRemove = do
  deleted <- deleteFrom profileRoles (\role' -> role' ! #profileId .== literal pid
         .&& role' ! #role .== literal roleToRemove)
  pure $ deleted > 0
