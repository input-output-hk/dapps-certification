{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Certification.API.Swagger where

import Servant.Swagger.UI
import Servant.API as Servant
import Control.Lens hiding ((.=))
import Servant.Swagger
import Data.Proxy
import Data.Swagger as SWG
import Plutus.Certification.API.Routes
import Data.Text
import GHC.TypeLits
import IOHK.Certification.Persistence ()

-- TODO: separate jwt auth from the plain auth
type UnnamedApi (auth :: Symbol)
     = VersionRoute
  :<|> VersionHeadRoute
  :<|> CreateRunRoute auth
  :<|> CreateRunOnCurrentProfileRoute auth
  :<|> GetRunRoute
  :<|> AbortRunRoute auth
  :<|> GetLogsRoute
  :<|> GetProfileRunsRoute auth
  :<|> GetCurrentProfileRunsRoute auth
  :<|> GetRunDetailsRoute
  :<|> GetCurrentProfileRoute auth
  :<|> GetProfileRoute auth
  :<|> UpdateCurrentProfileRoute auth
  :<|> UpdateProfileRoute auth
  :<|> CreateL1CertificationRoute auth
  :<|> GetCurrentProfileBalanceRoute auth
  :<|> GetProfileBalanceRoute auth
  :<|> WalletAddressRoute
  :<|> GitHubRoute
  :<|> GenerateGitHubTokenRoute
  :<|> GetGitHubClientId
  :<|> GetCurrentProfileSubscriptionsRoute auth
  :<|> GetProfileSubscriptionsRoute auth
  :<|> SubscribeRoute auth
  :<|> CancelCurrentProfilePendingSubscriptionsRoute auth
  :<|> CancelProfilePendingSubscriptionsRoute auth
  :<|> GetTiersRoute
  :<|> GetCurrentProfileActiveFeaturesRoute auth
  :<|> GetProfileActiveFeaturesRoute auth
  :<|> GetAdaUsdPriceRoute
  :<|> CreateAuditorReport auth
  :<|> GetCurrentProfileWalletAddressRoute auth
  :<|> GetProfileWalletAddressRoute auth
  :<|> UpdateProfileRolesRoute auth
  :<|> GetProfileRolesRoute auth
  :<|> GetAllProfileIdsByRole auth
  :<|> GetProfilesSummaryRoute auth
  :<|> GetRunTimeMetricsRoute auth
  :<|> GetSubscriptionsStartingInIntervalRoute auth
  :<|> GetSubscriptionsEndingInIntervalRoute auth
  :<|> GetAuditorReportMetrics auth
  :<|> GetProfileInvoicesRoute auth
  :<|> CancelInvoiceRoute auth
  :<|> GetAllInvoicesRoute auth
  :<|> CreateInvoiceRoute auth
  :<|> CreateSubscriptionInvoiceRoute auth
  :<|> DownloadInvoiceRoute auth

type UnnamedApiWithLogin (auth :: Symbol)
     = UnnamedApi auth
  :<|> LoginRoute
  :<|> ServerTimestamp

instance (HasSwagger sub) => HasSwagger (AuthProtect  "public-key" :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy (Servant.Header "Authorization" Text :> sub))

swaggerJson :: Swagger
swaggerJson = toSwagger (Proxy :: Proxy (UnnamedApi "public-key"))
  & info.title        .~ "Plutus Certification API"
  & info.SWG.version  .~ "1.0"
  & info.description  ?~ "This is an API for the Plutus Certification Service"

swaggerJsonWithLogin :: Swagger
swaggerJsonWithLogin = toSwagger (Proxy :: Proxy (UnnamedApiWithLogin "public-key"))
  & info.title        .~ "Plutus Certification API"
  & info.SWG.version  .~ "1.0"
  & info.description  ?~ "This is an API for the Plutus Certification Service"

type APIWithSwagger = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> API "public-key"
