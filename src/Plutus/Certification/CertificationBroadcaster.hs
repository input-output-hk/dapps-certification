{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Plutus.Certification.CertificationBroadcaster
  ( createCertification
  , renderTxBroadcasterSelector
  , TxBroadcasterSelector(..)
  ) where

import Conduit
import Control.Monad.Catch
import Data.Aeson
import Control.Monad.State.Strict
import Control.Exception
import Observe.Event
import Plutus.Certification.API as API hiding (createCertification)
import Control.Monad.Except
import Servant
import Servant.Client
import Data.Text as Text hiding (elem,replicate, last)
import Data.Text.Encoding
import Data.ByteString.Lazy.Char8 qualified as LSB
import Plutus.Certification.WalletClient (WalletArgs)
import Plutus.Certification.Server.Internal
import Observe.Event.Render.JSON

import qualified Plutus.Certification.WalletClient as Wallet
import qualified IOHK.Certification.Persistence as DB

data TxBroadcasterSelector f where
  CreateCertification :: TxBroadcasterSelector CreateCertificationField

data CreateCertificationField
  = CreateCertificationRunID !RunIDV1
  | CreateCertificationTxResponse !Wallet.TxResponse

renderTxBroadcasterSelector :: RenderSelectorJSON TxBroadcasterSelector

renderTxBroadcasterSelector CreateCertification = ("create-certification", \case
    CreateCertificationRunID rid -> ("run-id", toJSON rid)
    CreateCertificationTxResponse txResp -> ("tx-resp",toJSON txResp)
  )

-- caution: this function doesn't verify if the run has the proper status
createCertification :: (MonadMask m,MonadIO m, MonadError IOException m)
                    => EventBackend m r TxBroadcasterSelector
                    -> WalletArgs
                    -> DB.ProfileId
                    -> RunIDV1
                    -> m DB.Certification
createCertification eb wargs profileId rid@RunID{..} = withEvent eb CreateCertification \ev -> do
  addField ev (CreateCertificationRunID rid)

  -- getting required profile information before further processing
  (profile,dapp@DB.DApp{..}) <- getProfileAndDApp

  -- sync the run with the db and return the db-run information
  DB.Run{commitHash,reportContentId} <- getRun
  ipfsCid <- maybeToError "No report stored for this run" reportContentId

  -- create the certification object
  websiteUrl <- parseUrl (profile.website)
  FlakeRef{..} <- createFlakeRef dapp (CommitOrBranch commitHash)
  let certificate = Wallet.CertificationMetadata uuid (DB.IpfsCid ipfsCid) dappName websiteUrl
                    (profile.twitter) uri dappVersion

  -- broadcast the certification
  tx@Wallet.TxResponse{..} <- Wallet.broadcastTransaction wargs Nothing certificate
    >>= eitherToError show
  addField ev (CreateCertificationTxResponse tx)

  -- persist it into the db
  (DB.withDb . DB.createCertificate uuid txRespId =<< getNow)
    >>= maybeToError "Certification couldn't be persisted"
  where
    getRun = DB.withDb (DB.getRun uuid) >>= maybeToError "No Run"

    eitherToError f = either
      (throwException .  f)
      pure

    createFlakeRef DB.DApp{..} CommitOrBranch{..} = do
      when (Text.null dappOwner || Text.null dappRepo ) $ throwException "DApp owner or repo are empty"

      let uri = "github:" <> encodeUtf8 dappOwner <> "/" <> encodeUtf8 dappRepo <> "/" <> encodeUtf8 commitOrBranch
      eitherToError
        id
        (mimeUnrender (Proxy :: Proxy PlainText) (LSB.fromStrict uri))

    parseUrl website = eitherToError show
        (maybe (Right Nothing) (fmap Just . parseBaseUrl . unpack ) website)

    withDappNotAvailableMsg = maybeToError "DApp profile data not available"

    getProfileAndDApp = do
      DB.ProfileDTO{..} <- getProfileDTO
      dapp' <- withDappNotAvailableMsg dapp
      pure (profile,dapp')

    getProfileDTO = DB.withDb (DB.getProfile profileId)
        >>= maybeToError "Profile not found"

maybeToError :: MonadError IOException m => String -> Maybe a -> m a
maybeToError msg = maybe
  (throwException msg)
  pure

throwException :: (MonadError IOException m) => String -> m a
throwException = throwError . userError
