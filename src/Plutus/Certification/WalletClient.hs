{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Plutus.Certification.WalletClient
  ( TxResponse(..)
  , Amount(..)
  , WalletArgs(..)
  , broadcastTransaction
  ,CertificationMetadata(..)
  ) where

import Data.UUID
import GHC.Generics
import Data.Aeson
import Data.Proxy
import Network.HTTP.Client      hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Data.Text
import Data.Aeson.QQ
import Control.Monad.IO.Class
import IOHK.Certification.Persistence

data TxBody = forall a . (ToJSON a) => TxBody
  { passphrase :: !Text
  , address :: !Text
  , metadata :: !a
  }

instance ToJSON TxBody where
  toJSON (TxBody{..}) =
    [aesonQQ| {
        "passphrase": #{passphrase} ,
        "payments": [
            {
                "address": #{address},
                "amount": {
                    "quantity": 0,
                    "unit": "lovelace"
                }
            }
        ],
        "metadata": #{metadata}
    } |]

data Amount = Amount !Integer !Text deriving Show

instance FromJSON Amount where
  parseJSON = withObject "Amount" \o -> Amount
    <$> o .: "quantity"
    <*> o .: "unit"

instance ToJSON Amount where
  toJSON (Amount q u) = object [ "quantity" .= q  , "unit" .= u ]

data TxResponse = TxResponse
  { txRespAmount :: !Amount
  , txRespId :: !TxId
  } deriving Show

instance FromJSON TxResponse where
  parseJSON = withObject "TxResponse" \o -> TxResponse
    <$> o .: "amount"
    <*> o .: "id"

instance ToJSON TxResponse where
  toJSON TxResponse{..}= object [ "amount" .= txRespAmount , "id" .= txRespId ]

type API = "v2" :> "wallets"
         :> Capture "wallet-id" Text
         :> "transactions"
         :> ReqBody '[JSON] TxBody
         :> Verb 'POST 202 '[JSON] TxResponse

data WalletArgs = WalletArgs
  { walletId :: !Text
  , walletAddress :: !Text
  --TODO: this might not be safe to be passed as a
  , walletPassphrase :: !Text
  , walletAPIAddress :: !BaseUrl
  } deriving Show

data CertificationMetadata = CertificationMetadata
  { crtmId :: !UUID
  , crtmIpfsCid :: !IpfsCid
  , crtmProjectName :: !Text
  , crtmLink :: !(Maybe BaseUrl)
  , crtmTwitter :: !(Maybe Text)
  , crtmContractLink :: !URI
  , crtmVersion :: !Text
  } deriving Generic

splitString :: Int -> Text -> Value
splitString maxChars = toValue . chunksOf maxChars
    where
    toValue []     = toJSON ("" :: Text)
    toValue (x:[]) = toJSON x
    toValue xs     = toJSON xs

split64 :: Text -> Value
split64 = splitString 64

instance ToJSON CertificationMetadata where
  toJSON CertificationMetadata{..} =  object
    [ "id" .= crtmId
    , "ipfsCid" .= split64 (crtmIpfsCid.ipfsCid)
    , "projectName" .= split64 crtmProjectName
    , "link" .= fmap (split64 . pack . showBaseUrl ) crtmLink
    , "twitter" .= fmap split64 crtmTwitter
    , "contractLink" .= split64 (pack $ show crtmContractLink)
    , "version" .= split64 crtmVersion
    ]


broadcastTransaction :: (MonadIO m, ToJSON metadata)
                     => WalletArgs
                     -> metadata
                     -> m (Either ClientError TxResponse)
broadcastTransaction WalletArgs{..} metadata = liftIO $ do
  manager' <- newManager (if baseUrlScheme walletAPIAddress == Https then tlsManagerSettings else defaultManagerSettings)
  let settings  = (mkClientEnv manager' walletAPIAddress)
  let broadcastTx = client (Proxy :: Proxy API)
  let body = TxBody walletPassphrase walletAddress [aesonQQ| { "0": #{ metadata }} |]

  runClientM (broadcastTx walletId body ) settings

