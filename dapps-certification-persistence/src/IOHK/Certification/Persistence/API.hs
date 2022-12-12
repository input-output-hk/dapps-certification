{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module IOHK.Certification.Persistence.API where

import Database.Selda
import Database.Selda.SQLite
import IOHK.Certification.Persistence.Structure
import Data.Maybe
import Control.Monad

upsertProfile :: (MonadSelda m, MonadMask m) => Profile -> m (Maybe (ID Profile))
upsertProfile arg@Profile{..} = do
  upsert profiles
     (\profile -> profile ! #ownerAddress .== text ownerAddress)
     (`with`
      [ #dapp     := fromTextMaybe dapp -- TODO: don't know if we should update the dapp
      , #website  := fromTextMaybe website
      , #vendor   := fromTextMaybe vendor
      , #twitter  := fromTextMaybe twitter
      , #linkedin := fromTextMaybe linkedin
      --TODO: authors and contacts
      ])
     [#profileId (def :: ID Profile) arg]
  where
  fromTextMaybe = maybe null_ (just . text)

getProfileByAddress :: MonadSelda m => Text -> m (Maybe Profile)
getProfileByAddress address = listToMaybe <$> (query $ do
    p <- select profiles
    restrict (p ! #ownerAddress .== text address)
    pure p
  )

getProfileQ :: ID Profile -> Query t (Row t Profile)
getProfileQ pid = do
    p <- select profiles
    restrict (p ! #profileId .== literal pid )
    pure p

getProfile :: MonadSelda m => ID Profile -> m (Maybe Profile)
getProfile = fmap listToMaybe . query . getProfileQ


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

createRun :: MonadSelda m => UUID -> UTCTime -> Text -> UTCTime -> ID Profile -> m ()
createRun runId time repo commitDate pid = do
  void $ insert runs [Run runId time (Just time) time repo commitDate Queued pid]

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
    (\run -> (run ! #runId .== literal runId))
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
        (\run -> (run ! #runId .== literal runId))
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
