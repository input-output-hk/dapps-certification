{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}


{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Certification.API.Routes where

import Control.Applicative
import Servant.API as Servant
import Servant.API.Generic
import Servant.API.Verbs
import Data.Version
import Data.Aeson
import Data.Aeson.Encoding
import Network.URI
import Data.UUID
import Data.ByteString.Lazy.Char8 as BSL8
import Data.Time.LocalTime
import Data.Text as Text
import Data.Text.Encoding
import Data.Swagger
import IOHK.Certification.Interface
import Data.Time
import Data.Proxy
import Text.Read hiding (String)
import Plutus.Certification.WalletClient
import Control.Lens hiding ((.=))
import Plutus.Certification.GitHubClient (RepositoryInfo,AccessTokenGenerationResponse)
import Control.Arrow (ArrowChoice(left))
import GHC.TypeLits
import IOHK.Certification.SignatureVerification
  (COSEKey,COSESign1, decodeHex,encodeHex)
import Plutus.Certification.Metadata as Metadata
import Data.Int
import Data.HashMap.Strict.InsOrd as HM
import Network.HTTP.Media ((//), (/:))

import qualified Data.Swagger.Lens as SL
import qualified IOHK.Certification.Persistence as DB
import qualified Plutus.Certification.API.RunLog as API (RunLog(..))
import qualified Data.Aeson.KeyMap as KM
import IOHK.Certification.Persistence (ProfileId)
import Plutus.Certification.Metrics (RunTimeMetric)

type API (auth :: Symbol)  = NamedRoutes (NamedAPI auth)

type VersionRoute
   = "version"
  :> Description "Get the api version"
  :> Get '[JSON] VersionV1

type VersionHeadRoute
   = "version"
  :> Description "Get the api version (Response Headers only)"
  :> HeadNoContent

type CreateRunOnCurrentProfileRoute (auth :: Symbol)
   = "run"
  :> Description "Create a new testing run attached to the current profile"
  :> ReqBody '[PlainText,JSON] CreateRunOptions
  :> AuthProtect auth
  :> PostCreated '[OctetStream, PlainText, JSON] RunIDV1

type CreateRunRoute (auth :: Symbol)
   = "profile"
  :> Description "Create a new testing run attached to a different profile"
  :> ReqBody '[PlainText,JSON] CreateRunOptions
  :> Capture "id" ProfileId
  :> "run"
  :> AuthProtect auth
  :> PostCreated '[OctetStream, PlainText, JSON] RunIDV1

type GetRunRoute
   = "run"
  :> Description "Get the status of a run"
  :> Capture "id" RunIDV1
  :> Get '[JSON] RunStatusV1

type AbortRunRoute (auth :: Symbol)
   = "run"
  :> Description "Abort a run and deletes the history entry if query param is provided"
  :> Capture "id" RunIDV1
  :> QueryParam "delete" Bool
  :> AuthProtect auth
  :> DeleteNoContent

-- TODO: Deprecate or remove `action-type` in favor of `certification-stage`
type GetLogsRoute
   = "run"
  :> Description "Get the logs of a run"
  :> Capture "id" RunIDV1
  :> "logs"
  :> QueryParam "after" ZonedTime
  :> QueryParam "action-type" CertificationStage
  :> Get '[JSON] [API.RunLog]

type GetCurrentProfileRunsRoute (auth :: Symbol)
   = "run"
  :> Description "Query through multiple runs belonging to the current profile"
  :> AuthProtect auth
  :> QueryParam "after" UTCTime
  :> QueryParam "count" Int
  :> Get '[JSON] [DB.Run]

type GetProfileRunsRoute (auth :: Symbol)
   = "profile"
  :> Description "Query through multiple profile runs"
  :> Capture "id" ProfileId
  :> "runs"
  :> QueryParam "after" UTCTime
  :> QueryParam "count" Int
  :> AuthProtect auth
  :> Get '[JSON] [DB.Run]

type GetRunDetailsRoute
   = "run"
  :> Description "Get the details of a run"
  :> Capture "id" RunIDV1
  :> "details"
  :> Get '[JSON] DB.Run

type GetCurrentProfileRoute (auth :: Symbol)
   = GetProfileRoute' auth "current" "Get the current profile information"

type GetProfileRoute (auth :: Symbol)
   = GetProfileRoute' auth (Capture "id" ProfileId)  "Get the profile information"

type GetProfileRoute' (auth :: Symbol) p description
   = "profile"
  :> Description description
  :> p
  :> AuthProtect auth
  :> Get '[JSON] DB.ProfileDTO

type UpdateCurrentProfileRoute (auth :: Symbol) =
  UpdateProfileRoute' auth "current"
    "Update the current profile information"

type UpdateProfileRoute (auth :: Symbol) =
  UpdateProfileRoute' auth (Capture "id" ProfileId)
    "Update the profile information"

type UpdateProfileRoute' (auth :: Symbol) p description
   = "profile"
  :> Description description
  :> ReqBody '[JSON] ProfileBody
  :> p
  :> AuthProtect auth
  :> Put '[JSON] DB.ProfileDTO

type CreateL1CertificationRoute (auth :: Symbol)
   = "run"
  :> Description "Store the L1 Report into IPFS and broadcasts the Certificate onchain"
  :> AuthProtect auth
  :> Capture "id" RunIDV1
  :> "certificate"
  :> ReqBody '[JSON] CertificationInput
  :> QueryParam "dry-run" Bool
  :> Post '[JSON] Metadata.FullMetadata

type GetCurrentProfileBalanceRoute (auth :: Symbol)
   = Description "Get the current balance of the profile"
  :> GetProfileBalanceRoute' auth "current"

type GetProfileBalanceRoute (auth :: Symbol)
   = Description "Get the balance of a given profile"
  :> GetProfileBalanceRoute' auth (Capture "id" ProfileId)

type GetProfileBalanceRoute' (auth :: Symbol) p
   = "profile"
  :> p
  :> "balance"
  :> AuthProtect auth
  :> Get '[JSON] Int64

type WalletAddressRoute = "wallet-address"
  :> Description "Get the wallet address the backend operates with"
  :> Get '[JSON] WalletAddress

type GetCurrentProfileWalletAddressRoute (auth :: Symbol)
   = Description "Get the wallet address of the current profile"
  :> GetProfileWalletAddressRoute' auth "current"

type GetProfileWalletAddressRoute (auth :: Symbol)
   = Description "Get the wallet address of a given profile"
  :> GetProfileWalletAddressRoute' auth (Capture "id" ProfileId)

type GetProfileWalletAddressRoute' (auth :: Symbol) p
   = "profile"
  :> p
  :> "wallet-address"
  :> AuthProtect auth
  :> Get '[JSON] (Maybe (DB.WalletAddressStatus,WalletAddress))

type GitHubRoute
   = "repo"
  :> Description "Get the github repo information"
  :> Capture "owner" Text
  :> Capture "repo" Text
  :> Servant.Header "Authorization" ApiGitHubAccessToken
  :> Get '[JSON] RepositoryInfo

type GenerateGitHubTokenRoute
   = "github"
  :> "access-token"
  :> Description "Generate a github access token"
  :> Capture "code" Text
  :> Post '[JSON] AccessTokenGenerationResponse

type GetGitHubClientId
   = "github"
  :> "client-id"
  :> Description "Get the application client id"
  :> Get '[JSON] Text

type LoginRoute
   = "login"
  :> Description "Get a jwt token based on the provided credentials"
  :> ReqBody '[JSON] LoginBody
  :> Post '[JSON] Text

type ServerTimestamp
   = "server-timestamp"
  :> Description "Get the current server timestamp"
  :> Get '[JSON] Integer

type GetCurrentProfileSubscriptionsRoute (auth :: Symbol)
   = Description "Get the current profile subscriptions. Expiration isn't checked, so it's possible to get expired subscriptions"
  :> GetProfileSubscriptionsRoute' auth "current"

type GetProfileSubscriptionsRoute (auth :: Symbol)
   = Description "Get the profile subscriptions. Expiration isn't checked, so it's possible to get expired subscriptions"
  :> GetProfileSubscriptionsRoute' auth (Capture "id" ProfileId)

type GetProfileSubscriptionsRoute' (auth :: Symbol) p
   = "profile"
  :> QueryParam "just-enabled" Bool
  :> p
  :> "subscriptions"
  :> AuthProtect auth
  :> Get '[JSON] [DB.SubscriptionDTO]

--TODO: in the future we might want to create a new subscription
-- directly from support dashboard
type SubscribeRoute (auth :: Symbol) = "profile"
  :> Description "Create a new profile subscription"
  :> "current"
  :> "subscriptions"
  :> AuthProtect auth
  :> Capture "tier" Int64
  :> PostCreated '[JSON] DB.SubscriptionDTO

type CancelCurrentProfilePendingSubscriptionsRoute (auth :: Symbol) =
  CancelProfilePendingSubscriptionsRoute' auth "current"
    "Cancel the current profile pending subscriptions"

type CancelProfilePendingSubscriptionsRoute (auth :: Symbol) =
  CancelProfilePendingSubscriptionsRoute' auth (Capture "id" ProfileId)
    "Cancel a given profile pending subscriptions"

type CancelProfilePendingSubscriptionsRoute' (auth :: Symbol) p description
   = "profile"
  :> Description description
  :> p
  :> "subscriptions"
  :> "pending"
  :> AuthProtect auth
  :> Delete '[JSON] Int

type GetTiersRoute
   = "tiers"
  :> Description "Get the available tiers"
  :> Get '[JSON] [DB.TierDTO]

type GetCurrentProfileActiveFeaturesRoute (auth :: Symbol) =
  GetProfileActiveFeaturesRoute' auth "current"
    "Get the active features of the current profile"

type GetProfileActiveFeaturesRoute (auth :: Symbol) =
  GetProfileActiveFeaturesRoute' auth (Capture "id" ProfileId)
    "Get the active features of a given profile"

type GetProfileActiveFeaturesRoute' (auth :: Symbol) p description
   = "profile"
  :> Description description
  :> p
  :> "subscriptions"
  :> "active-features"
  :> AuthProtect auth
  :> Get '[JSON] [DB.FeatureType]

type GetAdaUsdPriceRoute
   = "ada-usd-price"
  :> Description "Get the current ADA/USD price"
  :> Get '[JSON] DB.AdaUsdPrice

type CreateAuditorReport (auth :: Symbol)
   = "auditor"
  :> Description "Fetches the auditor report L0 | L2"
  :> "reports"
  :> QueryParam "dry-run" Bool
  :> ReqBody '[JSON] Metadata.AuditorCertificationInput
  :> AuthProtect auth
  :> Post '[JSON] Metadata.FullMetadata

--------------------------------------------------------------------------------
-- | Roles

type UpdateProfileRolesRoute (auth :: Symbol)
   = "profile"
  :> Description "Update the roles of a given profile"
  :> Capture "id" ProfileId
  :> "roles"
  :> ReqBody '[JSON] [DB.UserRole]
  :> AuthProtect auth
  :> PutNoContent

type GetCurrentProfileRolesRoute (auth :: Symbol)
   = Description "Get current profile roles"
  :> GetProfileRolesRoute' auth "current"

type GetProfileRolesRoute (auth :: Symbol)
   = Description "Get the roles of a given profile"
  :> GetProfileRolesRoute' auth (Capture "id" ProfileId)

type GetProfileRolesRoute' (auth :: Symbol) p
   = "profile"
  :> p
  :> "roles"
  :> AuthProtect auth
  :> Get '[JSON] [DB.UserRole]

type GetAllProfileIdsByRole (auth :: Symbol)
   = "roles"
  :> Description "Get all profile ids by role"
  :> Capture "role" DB.UserRole
  :> "profiles"
  :> "summary"
  :> AuthProtect auth
  :> Get '[JSON] [DB.ProfileId]

--------------------------------------------------------------------------------
-- | HTML

type HTMLMain = "dev-panel" :>
  ( Get '[HTML] RawHtml
  :<|> "accounts" :> Get '[HTML] RawHtml
  :<|> "accounts" :> (Capture "profileId" DB.ProfileId :> Get '[HTML] RawHtml)
  :<|> "transactions" :> Get '[HTML] RawHtml
  :<|> "billing" :> Get '[HTML] RawHtml
  :<|> "invoice" :> (Capture "invoiceId" DB.InvoiceId :> Get '[HTML] RawHtml)
  :<|> "invoice" :> (Capture "invoiceId" DB.InvoiceId :> "pdf"
        :> Get '[OctetStream] (Headers
            '[ Servant.Header "Content-Type" String
             , Servant.Header "Content-Transfer-Encoding" String
             , Servant.Header "Content-Disposition" String
             ] PdfBytesString))
  )

data HTML = HTML

newtype RawHtml = RawHtml { unRaw :: Text }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = BSL8.fromStrict . encodeUtf8 . unRaw

--------------------------------------------------------------------------------
-- | GLOBAL ELEVATED ROUTES

type GetProfilesSummaryRoute  (auth :: Symbol)
  = "profiles"
  :> Description "Getting all profiles with their maximum role and their dapp if any"
  :> AuthProtect auth
  :> Get '[JSON] [DB.ProfileSummaryDTO]

newtype SlotSelector = SlotSelector (UTCTime, UTCTime)

newtype FlexibleSlotSelector = FlexibleSlotSelector (Maybe UTCTime, Maybe UTCTime)

data RunTimeArguments = RunTimeArguments
  { interval :: !SlotSelector
  , minRunTime :: !(Maybe NominalDiffTime)
  }

type GetRunTimeMetricsRoute (auth :: Symbol)
   = "metrics"
  :> Description "Get the run times"
  :> "run-times"
  :> ReqBody '[JSON] RunTimeArguments
  :> AuthProtect auth
  :> Post '[JSON] [RunTimeMetric]


type GetSubscriptionsStartingInIntervalRoute (auth :: Symbol)
  = "metrics"
  :> Description "Get all subscriptions created in a given interval"
  :> "subscriptions"
  :> "started"
  :> "in-interval"
  :> ReqBody '[JSON] SlotSelector
  :> AuthProtect auth
  :> Post '[JSON] [DB.SubscriptionDTO]

type GetSubscriptionsEndingInIntervalRoute (auth :: Symbol)
  = "metrics"
  :> Description "Get all subscriptions expiring in a given interval"
  :> "subscriptions"
  :> "ending"
  :> "in-interval"
  :> ReqBody '[JSON] SlotSelector
  :> AuthProtect auth
  :> Post '[JSON] [DB.SubscriptionDTO]

type GetAuditorReportMetrics (auth :: Symbol)
  = "metrics"
  :> Description "Get the auditor report metrics"
  :> "auditor-reports"
  :> ReqBody '[JSON] SlotSelector
  :> AuthProtect auth
  :> Post '[JSON] [DB.AuditorReportEvent]

--------------------------------------------------------------------------------
-- | TODO: INVOICING

type GetProfileInvoicesRoute (auth :: Symbol)
  = "profile"
  :> Description "Get all invoices for a given profile"
  :> Capture "id" ProfileId
  :> "invoices"
  :> AuthProtect auth
  :> Get '[JSON] [DB.InvoiceDTO]

type CancelInvoiceRoute (auth :: Symbol)
  = "invoices"
  :> Description "Cancel an invoice (creates a new cancellation invoice)"
  :> Capture "id" DB.InvoiceId
  :> "cancellation"
  :> AuthProtect auth
  :> Post '[JSON] DB.InvoiceDTO

type GetAllInvoicesRoute (auth :: Symbol)
  = "invoices"
  :> Description "Get all invoices in a given interval"
  :> ReqBody '[JSON] FlexibleSlotSelector
  :> AuthProtect auth
  :> Post '[JSON] [DB.InvoiceDTO]

type CreateInvoiceRoute (auth :: Symbol)
  = "profile"
  :> Description "Create an invoice attached to a profile"
  :> Capture "id" ProfileId
  :> "invoices"
  :> ReqBody '[JSON] DB.InvoiceBody
  :> AuthProtect auth
  :> Post '[JSON] DB.InvoiceDTO

type CreateSubscriptionInvoiceRoute (auth :: Symbol)
  =  "subscriptions"
  :> Description "Create an invoice attached to a subscription"
  :> Capture "id" DB.SubscriptionId
  :> "invoices"
  :> AuthProtect auth
  :> Post '[JSON] DB.InvoiceDTO


type DownloadInvoiceRoute auth = "invoice"
  :> Description "Download an invoice in pdf format"
  :> AuthProtect auth
  :> Capture "id" DB.InvoiceId
  :> Get '[OctetStream] (Headers
      '[ Servant.Header "Content-Type" String
       , Servant.Header "Content-Transfer-Encoding" String
       , Servant.Header "Content-Disposition" String
       ] PdfBytesString)

newtype PdfBytesString = PdfBytesString { unPdfBytesString :: ByteString }

instance ToSchema PdfBytesString where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "PdfBytesString") binarySchema

--------------------------------------------------------------------------------
-- | TYPES AND INSTANCES

instance FromJSON SlotSelector where
  parseJSON = withObject "SlotSelector" $ \o -> do
    from' <- o .: "from"
    to' <- o .: "to"
    pure $ SlotSelector (from', to')

instance ToJSON SlotSelector where
  toJSON (SlotSelector (from', to')) = object
    [ "from" .= from'
    , "to" .= to'
    ]

instance ToSchema SlotSelector where
  declareNamedSchema _ = do
    fromSchema <- declareSchemaRef (Proxy @UTCTime)
    toSchema' <- declareSchemaRef (Proxy @UTCTime)
    pure $ NamedSchema (Just "SlotSelector") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~ fromList
        [ ("from", fromSchema)
        , ("to", toSchema')
        ]
      & required .~ ["from", "to"]

instance FromJSON FlexibleSlotSelector where
  parseJSON = withObject "FlexibleSlotSelector" $ \o -> do
    from' <- o .:? "from"
    to' <- o .:? "to"
    pure $ FlexibleSlotSelector (from', to')

instance ToJSON FlexibleSlotSelector where
  toJSON (FlexibleSlotSelector (from', to')) = object
    [ "from" .= from'
    , "to" .= to'
    ]

instance ToSchema FlexibleSlotSelector where
  declareNamedSchema _ = do
    fromSchema <- declareSchemaRef (Proxy @UTCTime)
    toSchema' <- declareSchemaRef (Proxy @UTCTime)
    pure $ NamedSchema (Just "FlexibleSlotSelector") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~ fromList
        [ ("from", fromSchema)
        , ("to", toSchema')
        ]

instance FromJSON RunTimeArguments where
  parseJSON = withObject "RunTimeArguments" $ \o -> do
    interval <- o .: "interval"
    minRunTimeSeconds <- o .:? "minRunTime"
    let minRunTime = secondsToNominalDiffTime <$> minRunTimeSeconds
    pure RunTimeArguments{..}

instance ToJSON RunTimeArguments where
  toJSON RunTimeArguments{..} = object
    [ "interval" .= interval
    , "minRunTime" .= (nominalDiffTimeToSeconds <$> minRunTime)
    ]

instance ToSchema RunTimeArguments where
  declareNamedSchema _ = do
    slotSelectorSchema <- declareSchemaRef (Proxy @SlotSelector)
    minRunTimeSchema <- declareSchemaRef (Proxy @NominalDiffTime)
    pure $ NamedSchema (Just "RunTimeArguments") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("interval", slotSelectorSchema)
          , ("minRunTime", minRunTimeSchema)
          ]
      & required .~ ["interval"]

newtype ApiGitHubAccessToken = ApiGitHubAccessToken { unApiGitHubAccessToken :: GitHubAccessToken }
  deriving (Generic)

instance ToHttpApiData ApiGitHubAccessToken where
  -- | Convert a 'GitHubAccessToken' to a 'Text' value.
  toUrlPiece  = Text.pack . show . unApiGitHubAccessToken

instance FromHttpApiData ApiGitHubAccessToken where
  -- | Parse a 'GitHubAccessToken' from a 'Text' value.
  parseUrlPiece  = left Text.pack . fmap ApiGitHubAccessToken . ghAccessTokenFromText


instance FromHttpApiData DB.ProfileWalletAddress where
  parseUrlPiece  = left Text.pack . DB.mkPatternedText


instance FromHttpApiData (DB.ID a) where
  parseUrlPiece  = left Text.pack . maybe (Left "Invalid id") (Right . DB.toId) . readMaybe . Text.unpack

instance ToHttpApiData (DB.ID a) where
  toUrlPiece  = Text.pack . show . DB.fromId

data LoginBody = LoginBody
  { address :: !WalletAddress
  , key :: !COSEKey
  , signature :: !COSESign1
  , expiration :: !(Maybe Integer)
  } deriving (Generic)

instance ToSchema LoginBody where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    expirationScheme <- declareSchemaRef (Proxy :: Proxy Integer)
    return $ NamedSchema (Just "LoginBody") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("address", textSchema)
          , ("key", textSchema)
          , ("signature", textSchema)
          , ("expiration", expirationScheme)
          ]
      & required .~ ["address", "key", "signature"]

instance FromJSON LoginBody where
   parseJSON = withObject "LoginBody" $ \v -> do
    key <- decodeHex . encodeUtf8 <$> (v .: "key")
    signature <- decodeHex. encodeUtf8 <$> v .: "signature"
    address <- v .: "address"
    expiration <- v .:? "expiration" .!= Nothing
    case (key, signature) of
      (Right key', Right signature') ->
        pure $ LoginBody address key' signature' expiration
      (Left err, _) -> fail $ "key: " <> err
      (_, Left err) -> fail $ "signature: " <> err

instance ToJSON LoginBody where
  toJSON LoginBody{..} = object (
      [ "address" .= address
      , "key" .= decodeUtf8 (encodeHex key)
      , "signature" .= decodeUtf8 (encodeHex signature)
      ] ++ maybe [] (\exp' -> ["expiration" .= exp']) expiration
      )

newtype CertificateCreationResponse = CertificateCreationResponse
  { certCreationReportId :: Text
  }

-- TODO: separate jwt auth from the plain auth
data NamedAPI (auth :: Symbol) mode = NamedAPI
  { version :: mode :- VersionRoute
  , versionHead :: mode :- VersionHeadRoute
  , createRunOnCurrentProfile :: mode :- CreateRunOnCurrentProfileRoute auth
  , createRun :: mode :- CreateRunRoute auth
  , getRun :: mode :- GetRunRoute
  , abortRun :: mode :- AbortRunRoute auth
  , getLogs :: mode :- GetLogsRoute
  , getCurrentProfileRuns :: mode :- GetCurrentProfileRunsRoute auth
  , getProfileRuns :: mode :- GetProfileRunsRoute auth
  , getCurrentProfile :: mode :- GetCurrentProfileRoute auth
  , getProfile :: mode :- GetProfileRoute auth
  , updateCurrentProfile :: mode :- UpdateCurrentProfileRoute auth
  , updateProfile :: mode :- UpdateProfileRoute auth
  , getCurrentProfileWalletAddress :: mode :- GetCurrentProfileWalletAddressRoute auth
  , getProfileWalletAddress :: mode :- GetProfileWalletAddressRoute auth
  , createCertification :: mode :- CreateL1CertificationRoute auth
  , walletAddress :: mode :- WalletAddressRoute
  , getCurrentProfileBalance :: mode :- GetCurrentProfileBalanceRoute auth
  , getProfileBalance :: mode :- GetProfileBalanceRoute auth
  , getRunDetails :: mode :- GetRunDetailsRoute
  , getRepositoryInfo :: mode :- GitHubRoute
  , login :: mode :- LoginRoute
  , serverTimestamp :: mode :- ServerTimestamp
  , generateGitHubToken :: mode :- GenerateGitHubTokenRoute
  , getGitHubClientId :: mode :- GetGitHubClientId
  , getCurrentProfileSubscriptions :: mode :- GetCurrentProfileSubscriptionsRoute auth
  , getProfileSubscriptions :: mode :- GetProfileSubscriptionsRoute auth
  , subscribe :: mode :- SubscribeRoute auth
  , cancelCurrentProfilePendingSubscriptions ::
      mode :- CancelCurrentProfilePendingSubscriptionsRoute auth
  , cancelProfilePendingSubscriptions ::
      mode :- CancelProfilePendingSubscriptionsRoute auth
  , getAllTiers :: mode :- GetTiersRoute
  , getCurrentProfileActiveFeatures :: mode :- GetCurrentProfileActiveFeaturesRoute auth
  , getProfileActiveFeatures :: mode :- GetProfileActiveFeaturesRoute auth
  , getAdaUsdPrice :: mode :- GetAdaUsdPriceRoute
  , createAuditorReport :: mode :- CreateAuditorReport auth
  , updateProfileRoles :: mode :- UpdateProfileRolesRoute auth
  , getCurrentProfileRoles :: mode :- GetCurrentProfileRolesRoute auth
  , getProfileRoles :: mode :- GetProfileRolesRoute auth
  , getAllProfilesByRole :: mode :- GetAllProfileIdsByRole auth
  , getProfilesSummary :: mode :- GetProfilesSummaryRoute auth
  , getRunTimeMetrics :: mode :- GetRunTimeMetricsRoute auth
  , getSubscriptionsStartingInIntervalRoute :: mode :- GetSubscriptionsStartingInIntervalRoute auth
  , getSubscriptionsEndingInIntervalRoute :: mode :- GetSubscriptionsEndingInIntervalRoute auth
  , getAuditorReportMetrics :: mode :- GetAuditorReportMetrics auth
  , htmx :: mode :- HTMLMain
  , getProfileInvoices :: mode :- GetProfileInvoicesRoute auth
  , getAllInvoices :: mode :- GetAllInvoicesRoute auth
  , cancelInvoice :: mode :- CancelInvoiceRoute auth
  , createInvoice :: mode :- CreateInvoiceRoute auth
  , createSubscriptionInvoice :: mode :- CreateSubscriptionInvoiceRoute auth
  , downloadInvoice :: mode :- DownloadInvoiceRoute auth
  } deriving stock Generic

newtype VersionV1 = VersionV1 { version :: Version } deriving (Generic)

versionV1Branch :: VersionV1 -> [Int]
versionV1Branch v = versionBranch $ v.version

instance ToJSON VersionV1 where
  toEncoding = toEncoding . versionV1Branch
  toJSON = toJSON . versionV1Branch

instance FromJSON VersionV1 where
  parseJSON v = VersionV1 . makeVersion <$> parseJSON v

newtype FlakeRefV1 = FlakeRef { uri :: URI }
                   deriving(Generic)
newtype CommitOrBranch = CommitOrBranch { commitOrBranch :: Text }
                   deriving(Generic)

instance FromJSON CommitOrBranch where
  parseJSON = withText "CommitOrBranch" $ \t -> pure $ CommitOrBranch t

instance ToJSON CommitOrBranch where
  toJSON (CommitOrBranch t) = toJSON t

instance ToJSON FlakeRefV1 where
  toJSON (FlakeRef uri) = toJSON $ show uri

data CreateRunOptions = CreateRunOptions {
  croCommitOrBranch :: CommitOrBranch,
  croCertArgs :: CertifyArgs
  }

instance FromJSON CreateRunOptions where
  parseJSON = withObject "CreateRunOptions" $ \v -> do
    commitOrBranch <- v .: "commitOrBranch"
    certArgs <- v .: "certArgs"
    pure $ CreateRunOptions commitOrBranch certArgs

instance ToJSON CreateRunOptions where
  toJSON (CreateRunOptions commitOrBranch certArgs) = object [
    "commitOrBranch" .= commitOrBranch,
    "certArgs" .= certArgs
    ]

instance ToSchema CreateRunOptions where
  declareNamedSchema _ = do
    commitOrBranchSchema <- declareSchemaRef (Proxy :: Proxy CommitOrBranch)
    certArgsSchema <- declareSchemaRef (Proxy :: Proxy CertifyArgs)
    pure $ NamedSchema (Just "CreateRunOptions") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("commitOrBranch", commitOrBranchSchema)
          , ("certArgs", certArgsSchema)
          ]
      & required .~ ["commitOrBranch", "certArgs"]

instance MimeUnrender PlainText CommitOrBranch where
  mimeUnrender _ = Right . CommitOrBranch . decodeUtf8 . BSL8.toStrict

instance MimeUnrender PlainText CreateRunOptions where
  mimeUnrender _ = Right . flip CreateRunOptions DefaultCertifyArgs . CommitOrBranch . decodeUtf8 . BSL8.toStrict

instance MimeUnrender PlainText FlakeRefV1 where
  mimeUnrender _ uriBs = case parseAbsoluteURI uriStr of
      Just u -> case u.uriScheme of
        "github:" -> Right $ FlakeRef u
        s -> Left $ "URI '" ++ uriStr ++ "' must be a github: flakeref, not '" ++ s ++ "'"
      Nothing -> Left $ "couldn't not parse '" ++ uriStr ++ "' as an absolute URI"
    where
      uriStr = BSL8.unpack uriBs

instance MimeRender PlainText FlakeRefV1 where
  mimeRender _ ref = BSL8.pack $ uriToString id ref.uri ""

instance MimeRender PlainText CommitOrBranch where
  mimeRender _ = BSL8.pack . Text.unpack . commitOrBranch

instance MimeRender PlainText CreateRunOptions where
  mimeRender _ = BSL8.pack . Text.unpack . commitOrBranch . croCommitOrBranch

newtype RunIDV1 = RunID { uuid :: UUID }
  deriving newtype (FromHttpApiData, ToHttpApiData, ToJSON )
  deriving stock (Generic,Show,Eq)

instance MimeRender PlainText RunIDV1 where
  mimeRender _ rid = toLazyASCIIBytes rid.uuid

instance MimeRender OctetStream RunIDV1 where
  mimeRender _ rid = toByteString rid.uuid

instance MimeUnrender OctetStream RunIDV1 where
  mimeUnrender _ ridbs = case fromByteString ridbs of
    Just rid -> pure $ RunID rid
    Nothing -> Left $ "couldn't parse '" ++ BSL8.unpack ridbs ++ "' as a run ID"

instance MimeRender OctetStream PdfBytesString where
  mimeRender _ (PdfBytesString bs) = bs

instance MimeUnrender OctetStream PdfBytesString where
  mimeUnrender _ bs = Right $ PdfBytesString bs

instance MimeUnrender HTML RawHtml where
  mimeUnrender _ = Right . RawHtml . decodeUtf8 . BSL8.toStrict

data StepState
  = Running
  | Failed deriving stock (Eq, Ord)
  deriving (Generic)

instance ToJSON StepState where
  toJSON Running = String "running"
  toJSON Failed = String "failed"

  toEncoding Running = text "running"
  toEncoding Failed = text "failed"

instance FromJSON StepState where
  parseJSON = withText "StepState" go
    where
      go t
        | t == "running" = pure Running
        | t == "failed" = pure Failed
        | otherwise = fail $ "unknown step state " ++ show t

data CertifyingStatus = CertifyingStatus
  { certifyingState :: !StepState
  , certifyingProgress :: !(Maybe Progress)
  , certifyingPlan :: !(Maybe [CertificationTask])
  } deriving (Generic)

data IncompleteRunStatus
  = Queued
  | Preparing !StepState
  | Building !StepState
  | Certifying !CertifyingStatus
  deriving (Generic)

instance ToJSON IncompleteRunStatus where
  toJSON Queued = object
    [ "status" .= String "queued"
    ]
  toJSON (Preparing s) = object
    [ "status" .= ("preparing" :: Text)
    , "state"  .= s
    ]
  toJSON (Building s) = object
    [ "status" .= ("building" :: Text)
    , "state"  .= s
    ]
  toJSON (Certifying (CertifyingStatus {..})) = object $
      [ "status" .= ("certifying" :: Text)
      , "state"  .= certifyingState
      ] ++ maybeProgress ++ maybePlan
    where
      maybeProgress
        | Just p <- certifyingProgress = [ "progress" .= p ]
        | otherwise = []
      maybePlan
        | Just p <- certifyingPlan = [ "plan" .= p ]
        | otherwise = []

  toEncoding Queued = pairs
    ( "status" .= ("queued" :: Text)
    )
  toEncoding (Preparing s) = pairs
    ( "status" .= ("preparing" :: Text)
   <> "state"  .= s
    )
  toEncoding (Building s) = pairs
    ( "status" .= ("building" :: Text)
   <> "state"  .= s
    )
  toEncoding (Certifying (CertifyingStatus {..})) = pairs
    ( "status" .= ("certifying" :: Text)
   <> "state"  .= certifyingState
   <> maybe mempty ("progress" .=) certifyingProgress
   <> maybe mempty ("plan" .=) certifyingPlan
    )

instance FromJSON IncompleteRunStatus where
  parseJSON = withObject "IncompleteRunStatus" \o -> do
    status <- o .: "status"
    if | status == ("queued" :: Text) -> pure Queued
       | status == "preparing" -> Preparing <$> o .: "state"
       | status == "building" -> Building <$> o .: "state"
       | status == "certifying" -> Certifying <$> (CertifyingStatus
           <$> o .: "state"
           <*> o .:! "progress"
           <*> o .:! "plan")
       | otherwise -> fail $ "unknown status " ++ show status

data RunStatusV1
  = Incomplete !IncompleteRunStatus
  | Finished !CertificationResult
  deriving (Generic)

instance ToJSON RunStatusV1 where
  toJSON (Incomplete i) = toJSON i
  toJSON (Finished v) = object
    [ "status" .= ("finished" :: Text)
    , "result" .= v
    ]

  toEncoding (Incomplete i) = toEncoding i
  toEncoding (Finished v) = pairs
    ( "status" .= ("finished" :: Text)
   <> "result" .= v
    )
instance FromJSON RunStatusV1 where
  parseJSON v = parseIncomplete v <|> parseFinished v
    where
      parseIncomplete v' = Incomplete <$> parseJSON v'
      parseFinished = withObject "RunStatusV1" \o -> do
        status <- o .: "status"
        if status == ("finished" :: Text)
          then Finished <$> o .: "result"
          else fail $ "unknown status " ++ show status
--
-- | Certification job stages
data CertificationStage
  = Generate
  | Build
  | Certify
  deriving stock (Read,Show,Eq,Generic)

instance FromHttpApiData CertificationStage where
  parseUrlPiece "generate" = Right Generate
  parseUrlPiece "build"    = Right Build
  parseUrlPiece "certify"  = Right Certify
  parseUrlPiece _          = Left "Unknown CertificationStage"

instance ToHttpApiData CertificationStage where
  toUrlPiece Generate = "generate"
  toUrlPiece Build    = "build"
  toUrlPiece Certify  = "certify"

instance FromHttpApiData DB.UserRole where
  parseUrlPiece "support" = Right DB.Support
  parseUrlPiece "admin"   = Right DB.Admin
  parseUrlPiece "no-role" = Right DB.NoRole
  parseUrlPiece _         = Left "Unknown UserRole"

instance ToHttpApiData DB.UserRole where
  toUrlPiece DB.Admin   = "admin"
  toUrlPiece DB.Support = "support"
  toUrlPiece DB.NoRole  = "no-role"


instance ToSchema VersionV1
instance ToSchema RunStatusV1
instance ToSchema StepState
instance ToSchema CertifyingStatus
instance ToSchema RunIDV1
instance ToParamSchema RunIDV1
instance ToParamSchema CertificationStage

instance ToParamSchema ApiGitHubAccessToken where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & maxLength ?~ 40
    -- we use SL qualified because of an issue
    -- of parsing for the hlint. it seems to be
    -- some kind of bug
    & SL.pattern ?~ DB.ghAccessTokenPattern


instance ToSchema ApiGitHubAccessToken where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "ApiGitHubAccessToken") $ mempty
      & type_ ?~ SwaggerString
      & maxLength ?~ 40
      & SL.pattern ?~ DB.ghAccessTokenPattern

instance ToSchema FlakeRefV1  where
   declareNamedSchema _ = do
    return $ NamedSchema (Just "FlakeRefV1") $ mempty
      & type_ ?~ SwaggerString

instance ToSchema CommitOrBranch  where
   declareNamedSchema _ = do
    return $ NamedSchema (Just "CommitOrBranch") $ mempty
      & type_ ?~ SwaggerString

instance ToSchema IncompleteRunStatus where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

--NOTE: we keep Enum derivation explicitly to nail down the right action order
-- and also make it future-proof for any data constructors rearrangements
instance Enum CertificationStage where
  fromEnum Generate = 0
  fromEnum Build    = 1
  fromEnum Certify  = 2

  toEnum 0 = Generate
  toEnum 1 = Build
  toEnum 2 = Certify
  toEnum _ = errorWithoutStackTrace "Plutus.Certification.API.CertificationStage.toEnum: bad argument"

instance Ord CertificationStage where
  a <= b = fromEnum a <= fromEnum b

data ProfileBody = ProfileBody
  { profile :: !DB.Profile
  , dapp    :: !(Maybe DB.DApp)
  } deriving (Show,Eq)

instance ToSchema ProfileBody where
  declareNamedSchema _ = do
    profileDtoSchema <- declareSchema (Proxy :: Proxy DB.ProfileDTO)
    pure $ NamedSchema (Just "ProfileBody") $ profileDtoSchema
        -- remove address from properties
        & properties %~ HM.delete "address" . HM.delete "role"
        -- remove address from required
        & required %~ Prelude.filter (/= "address")

dummyAddress :: Text
dummyAddress = "addr100000000000000000000000000000000000000000000000000000000"

instance ToJSON ProfileBody where
  toJSON (ProfileBody p dapp) = Object (x <> y)
    where
    address :: DB.ProfileWalletAddress =
      case DB.mkPatternedText dummyAddress of
         Right addr -> addr
         Left err   -> error $ "failed to create dummy address: " <> err
    profile = p { DB.ownerAddress = address }
    x = KM.fromList [ "dapp" .= dapp ]
    y = case toJSON profile of
      Object obj -> obj
      _          -> KM.empty

instance FromJSON ProfileBody where
  parseJSON = withObject "ProfileBody" $ \v -> do
    -- if there isn't an address add a dummy one
    address <- v .:? "address" .!= dummyAddress
    let v' = KM.insert "address" (toJSON address) v
    dapp <- v .:? "dapp"
    ProfileBody
      <$> parseJSON (Object v')
      <*> pure dapp
