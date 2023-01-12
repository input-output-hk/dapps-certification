{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module IOHK.Certification.Persistence.API where

import Database.Selda
import Database.Selda.SQLite
import IOHK.Certification.Persistence.Structure
import Data.Maybe
import Control.Monad

upsertProfile :: (MonadSelda m, MonadMask m) => Profile -> Maybe DApp -> m (Maybe (ID Profile))
upsertProfile profile@Profile{..} dappM = do
  void $ upsert profiles
     (\p -> p ! #ownerAddress .== text ownerAddress)
     (`with`
      [ #website  := fromTextMaybe website
      , #vendor   := fromTextMaybe vendor
      , #twitter  := fromTextMaybe twitter
      , #linkedin := fromTextMaybe linkedin
      --TODO: authors and contacts
      ])
     [#profileId (def :: ID Profile) profile]
  -- we query this because upsert returns id only when inserts
  profileIdM <- getProfileId ownerAddress
  forM_ profileIdM $ \pid -> case dappM of
    Nothing -> do
        void $ deleteFrom dapps (\dapp -> dapp ! #dappId .== literal pid)
    Just dapp@DApp{..} -> do
      void $ upsert dapps
          (\dapp' -> dapp' ! #dappId .== literal pid)
          (`with`
           [ #dappName := text dappName
           , #dappOwner := text dappOwner
           , #dappRepo := text dappRepo
           , #dappVersion := text dappVersion
           , #dappId := literal profileId
           ]
          )
          [dapp { dappId = pid }]
  pure profileIdM
  where
  fromTextMaybe = maybe null_ (just . text)

getProfileByAddress :: MonadSelda m => Text -> m (Maybe Profile)
getProfileByAddress address = listToMaybe <$> query (do
    p <- select profiles
    restrict (p ! #ownerAddress .== text address)
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

getProfile :: MonadSelda m => ID Profile -> m (Maybe ProfileDTO)
getProfile pid = fmap (fmap toProfileDTO . listToMaybe ) $ query $ getProfileQ pid

getProfileDApp pid = fmap (listToMaybe ) $ query $ getProfileDAppQ pid

toProfileDTO :: (Profile :*: Maybe DApp) -> ProfileDTO
toProfileDTO (profile :*: dapp) = ProfileDTO{..}

getProfileIdQ:: Text -> Query t (Col t (ID Profile))
getProfileIdQ  address = do
  p <- select profiles
  restrict (p ! #ownerAddress .== text address)
  pure (p ! #profileId)

getProfileId :: MonadSelda m => Text -> m (Maybe ProfileId)
getProfileId = fmap listToMaybe . query . getProfileIdQ

getProfileAddressQ :: ID Profile -> Query t (Col t Text)
getProfileAddressQ  pid = do
  p <- select profiles
  restrict (p ! #profileId .== literal pid)
  pure (p ! #ownerAddress)

getProfileAddress :: MonadSelda m => ID Profile -> m (Maybe Text)
getProfileAddress = fmap listToMaybe . query . getProfileAddressQ

createRun :: MonadSelda m
          => UUID
          -> UTCTime
          -> Text
          -> UTCTime
          -> CommitHash
          -> ID Profile
          -> m ()
createRun runId time repo commitDate commitHash pid = do
  void $ insert runs [Run runId time (Just time) time repo commitDate commitHash Queued pid]

getRunOwnerQ :: UUID -> Query t (Col t (ID Profile))
getRunOwnerQ runId = do
    p <- select runs
    restrict (p ! #runId .== literal runId )
    pure (p ! #profileId)

getRunOwner :: MonadSelda m => UUID -> m (Maybe (ID Profile))
getRunOwner = fmap listToMaybe . query . getRunOwnerQ

updateFinishedRun :: MonadSelda m => UUID -> Bool -> UTCTime -> m Int
updateFinishedRun runId succeeded time= do
  update runs
    (\run -> (run ! #runId .== literal runId) .&& (run ! #runStatus .== literal Queued))
    (`with`
      [ #runStatus := literal (if succeeded then Succeeded else Failed)
      , #syncedAt := literal time
      , #finishedAt := literal (Just time)
      ]
    )

syncRun :: MonadSelda m => UUID ->  UTCTime -> m Int
syncRun runId time= update runs
    (\run -> run ! #runId .== literal runId)
    (`with` [ #syncedAt := literal time ])

createCertificate :: (MonadSelda m,MonadMask m) => UUID -> Text -> UTCTime -> m (Maybe Certification)
createCertificate runId ipfsCID time = transaction $ do
  result <- query $ do
    run <- select runs
    restrict (run ! #runId .== literal runId)
    restrict ( run ! #runStatus .== literal Succeeded)
    pure run
  case result of
    [_] -> do
      void $ update runs
        (\run -> run ! #runId .== literal runId)
        (`with` [ #runStatus := literal Certified
                , #syncedAt := literal time
                ])
      let cert = Certification def ipfsCID time runId
      certId <- insertWithPK certifications [cert]
      pure $ Just $ cert { certId = certId}
    _ -> pure Nothing

getCertificationQuery :: UUID -> Query t (Row t Certification)
getCertificationQuery runID = do
    c <- select certifications
    restrict (c ! #certRunId .== literal runID )
    pure c

getCertification :: MonadSelda m => UUID -> m (Maybe Certification)
getCertification = fmap listToMaybe . query . getCertificationQuery

getRuns :: MonadSelda m => ID Profile -> Maybe UTCTime -> Maybe Int -> m [Run]
getRuns pid afterM topM = query $
  case topM of
    Just top -> limit 0 top select'
    Nothing -> select runs
  where
  select' = do
    run <- select runs
    restrict (run ! #profileId .== literal pid)
    case afterM of
      Just after -> restrict (run ! #created .< literal after)
      Nothing -> pure ()
    order (run ! #created) descending
    pure run

--TODO: replace this with a proper configuration
withDb :: (MonadIO m, MonadMask m) => SeldaT SQLite m a -> m a
withDb = withSQLite "certification.sqlite"