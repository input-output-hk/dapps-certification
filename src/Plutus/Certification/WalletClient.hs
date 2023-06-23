{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}

module Plutus.Certification.WalletClient
  ( TxResponse(..)
  , Amount(..)
  , WalletArgs(..)
  , broadcastTransaction
  , getTransactionList
  , CertificationMetadata(..)
  , WalletAddress
  , WalletTransaction(..)
  , Direction(..)
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.QQ
import           Data.Proxy
import           Data.Text
import           Data.UUID
import           GHC.Generics
import           IOHK.Certification.Persistence
import           Network.HTTP.Client                           hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Plutus.Certification.WalletClient.Transaction
import           Servant.API
import           Servant.Client
import           Data.Int

data TxBody = forall a . (ToJSON a) => TxBody
  { passphrase :: !Text
  , address    :: !Text
  , metadata   :: !a
  }

instance ToJSON TxBody where
  toJSON :: TxBody -> Value
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
  , txRespId     :: !TxId
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
         :>( ReqBody '[JSON] TxBody :> Verb 'POST 202 '[JSON] TxResponse
           :<|> Get '[JSON] [WalletTransaction]
           )

type WalletAddress = Text
data WalletArgs = WalletArgs
  { walletId         :: !Text
  , walletAddress    :: !WalletAddress
  --TODO: this might not be safe to be passed as a
  , walletPassphrase :: !Text
  , walletAPIAddress :: !BaseUrl
  , walletCertificationPrice :: !Int64
  } deriving Show

data CertificationMetadata = CertificationMetadata
  { crtmId           :: !UUID
  , crtmIpfsCid      :: !IpfsCid
  , crtmProjectName  :: !Text
  , crtmLink         :: !(Maybe BaseUrl)
  , crtmTwitter      :: !(Maybe Text)
  , crtmContractLink :: !URI
  , crtmVersion      :: !Text
  } deriving Generic

splitString :: Int -> Text -> Value
splitString maxChars = toValue . chunksOf maxChars
    where
    toValue []  = toJSON ("" :: Text)
    toValue [x] = toJSON x
    toValue xs  = toJSON xs

split64 :: Text -> Value
split64 = splitString 64

instance ToJSON CertificationMetadata where
  toJSON CertificationMetadata{..} =  object $
    [ "id" .= crtmId
    , "ipfsCid" .= split64 (crtmIpfsCid.ipfsCid)
    , "projectName" .= split64 crtmProjectName
    , "contractLink" .= split64 (pack $ show crtmContractLink)
    , "version" .= split64 crtmVersion
    ] ++ maybe [] (\x -> [ "twitter" .= split64 x]) crtmTwitter
      ++ maybe [] (\x -> [ "link" .= (split64 . pack . showBaseUrl $ x )]) crtmLink

mkClient :: Text -> (TxBody -> ClientM TxResponse) :<|> ClientM [WalletTransaction]
mkClient = client (Proxy :: Proxy API)

mkSettings :: MonadIO m => BaseUrl -> m ClientEnv
mkSettings walletAPIAddress = liftIO $ do
  manager' <- newManager (if baseUrlScheme walletAPIAddress == Https then tlsManagerSettings else defaultManagerSettings)
  pure (mkClientEnv manager' walletAPIAddress)

type MetadataKey = Int
broadcastTransaction :: (MonadIO m, ToJSON metadata)
                     => WalletArgs
                     -> MetadataKey
                     -> metadata
                     -> m (Either ClientError TxResponse)
broadcastTransaction WalletArgs{..} metadataKey metadata = liftIO $ do
  settings <- mkSettings walletAPIAddress
  let broadcastTx :<|> _ = mkClient walletId
      metadataKeyStr = show metadataKey
  let body = TxBody walletPassphrase walletAddress [aesonQQ| { $metadataKeyStr: #{ metadata }} |]
  runClientM (broadcastTx body ) settings

getTransactionList :: (MonadIO m)
                   => WalletArgs
                   -> m (Either ClientError [WalletTransaction])
getTransactionList WalletArgs{..} = liftIO $ do
  settings <- mkSettings walletAPIAddress
  let _ :<|> getList = mkClient walletId
  runClientM getList settings
