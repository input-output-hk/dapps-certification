{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GADTs              #-}

module Plutus.Certification.CoinGeckoClient where

import Servant.API
import Servant.Client
import Data.Functor
import Data.Fixed
import Data.Aeson

import Observe.Event.Render.JSON
import Observe.Event.Backend
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Catch (MonadMask)
import Observe.Event
import Network.HTTP.Client hiding (Proxy)

import Network.HTTP.Client.TLS
import Data.Proxy

newtype CoinGeckoResponse = CoinGeckoResponse { usdPrice :: Micro } deriving (Show)

type API = "coins"
         :> Capture "id" String
         :> QueryParam "tickers" Bool
         :> QueryParam "market_data" Bool
         :> QueryParam "community_data" Bool
         :> QueryParam "developer_data" Bool
         :> QueryParam "sparkline" Bool
         :> Get '[JSON] CoinGeckoResponse

-- this is how we access the usd price => .market_data.current_price.usd
instance FromJSON CoinGeckoResponse where
  parseJSON = withObject "CoinGeckoResponse" \o ->
    o  .: "market_data"    >>=
      (.: "current_price") >>=
      (.: "usd")           <&>
      CoinGeckoResponse

data CoinGeckoClientSelector f where
  FetchAdaPrice :: CoinGeckoClientSelector (Either ClientError Micro)

renderCoinGeckoClientSelector :: RenderSelectorJSON CoinGeckoClientSelector
renderCoinGeckoClientSelector FetchAdaPrice = ("coin-gecko-fetch-price",\case
  Left err -> ("http-error",toJSON (show err))
  Right price -> ("ada-usd-price",toJSON price)
  )

getAdaPrice :: (MonadIO m,MonadMask m)
            => EventBackend m r CoinGeckoClientSelector
            -> m (Either ClientError Micro)
getAdaPrice eb = withEvent eb FetchAdaPrice $ \ev -> do
  manager' <- liftIO $ newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager'
                  (BaseUrl Https "api.coingecko.com" 443 "api/v3")
  let clientM = client (Proxy :: Proxy API)
                  "cardano"
                  (Just False)                  -- tickers
                  (Just True)                   -- market_data
                  (Just False)                  -- community_data
                  (Just False)                  -- developer_data
                  (Just False)                  -- sparkline

  resp <- liftIO (runClientM clientM clientEnv) -- extract the usd price
          <&> fmap usdPrice
  addField ev resp
  pure resp
