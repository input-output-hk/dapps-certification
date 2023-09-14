{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import Plutus.Certification.API
import Servant.Client hiding (manager)
import Servant.Client.Core.BaseUrl
import Servant.Client.Core
import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Header
import Network.HTTP.Client.TLS
import Options.Applicative
import Control.Exception hiding (handle)
import Data.UUID as UUID
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as LBS (putStrLn)
import Data.Coerce
import Network.URI hiding (scheme)
import Servant.API hiding (addHeader)
import Data.Aeson
import Data.Time.LocalTime
import Data.Time
import Data.Text as Text
import IOHK.Certification.Actions (gitHubAccessTokenParser)
import Data.Int

import qualified Data.ByteString.Base16 as Hexa
import GHC.TypeLits (KnownSymbol)
import Plutus.Certification.Metadata

newtype PublicKey = PublicKey { unPublicKey :: ByteString }
newtype JWT = JWT { unJWT :: ByteString }

data Auth = PublicKeyAuth PublicKey | JWTAuth JWT

flakeRefReader :: ReadM FlakeRefV1
flakeRefReader = do
  urlStr <- str
  case parseAbsoluteURI urlStr of
    Just u -> case u.uriScheme of
      "github:" -> pure $ FlakeRef u
      scheme -> readerError $ "URI '" ++ urlStr ++ "' must be a github: flakeref, not '" ++ scheme ++ "'"
    Nothing -> readerError $ "couldn't not parse '" ++ urlStr ++ "' as an absolute URI"

createRunParser :: Parser CreateRunArgs
createRunParser = CreateRunArgs
  <$> (CommitOrBranch <$> option str
    ( metavar "REF"
   <> help "the flake reference pointing to the repo to build"
    ))
  <*> authParser

authParser :: Parser Auth
authParser = (PublicKeyAuth <$> publicKeyParser) <|> (JWTAuth <$> jwtParser)

createRunInfo :: ParserInfo CreateRunArgs
createRunInfo = info createRunParser
  ( fullDesc
 <> header "plutus-certification-client run create — Create a new testing run"
  )

getRunParser :: Parser RunIDV1
getRunParser = argument (maybeReader (coerce . UUID.fromString))
  ( metavar "RUN_ID"
 <> help "the ID of the run"
  )

publicKeyParser :: Parser PublicKey
publicKeyParser = PublicKey <$> option str
  ( long "public-key"
 <> metavar "PUB_KEY"
 <> help "wallet public Key"
  )

jwtParser :: Parser JWT
jwtParser = JWT <$> option str
  ( long "auth-token"
 <> metavar "JWT"
 <> help "authorization token provided by `plutus-certification-client login` "
  )

getRunInfo :: ParserInfo RunIDV1
getRunInfo = info getRunParser
  ( fullDesc
 <> header "plutus-certification-client run get — Get the status of a run"
  )

abortRunInfo :: ParserInfo AbortRunArgs
abortRunInfo = info abortRunParser
  ( fullDesc
 <> header "plutus-certification-client run abort — Abort a run"
  )

getLogsParser :: Parser GetLogsArgs
getLogsParser = GetLogsArgs
  <$> getRunParser
  <*> optional (option generalReader
        ( long "after"
       <> metavar "AFTER"
       <> help "getting all the logs following a certain timestamp"
        ))
  <*> optional (option auto
        ( long "action-type"
       <> metavar "TYPE"
       <> help "filter logs by action-type (Generate/Build/Certify)"
        ))

getRunsParser :: Parser GetRunsArgs
getRunsParser = GetRunsArgs
  <$> authParser
  <*> optional (option generalReader
        ( long "after"
       <> metavar "AFTER"
       <> help "getting all the runs following a certain timestamp"
        ))
  <*> optional (option auto
        ( long "max-count"
       <> metavar "COUNT"
       <> help "maximum number or runs to fetch"
        ))

abortRunParser :: Parser AbortRunArgs
abortRunParser = AbortRunArgs
  <$> getRunParser
  <*> authParser
  <*> optional ( option auto
        ( long "delete-run"
       <> metavar "DELETE_RUN"
       <> help "to delete the run from db as well"
        )
      )

generalReader :: FromHttpApiData a => ReadM a
generalReader = do
  urlStr <- str
  case parseUrlPiece urlStr of
    Right u -> pure u
    Left t -> readerError $ Text.unpack t

getLogsInfo :: ParserInfo GetLogsArgs
getLogsInfo = info getLogsParser
  ( fullDesc
 <> header "plutus-certification-client run get-logs — Get the logs of a run"
  )

getRunsInfo :: ParserInfo GetRunsArgs
getRunsInfo = info getRunsParser
  ( fullDesc
 <> header "plutus-certification-client run get-many — Get many runs"
  )

getCertificationInfo :: ParserInfo RunIDV1
getCertificationInfo = info getRunParser
  ( fullDesc
 <> header "plutus-certification-client run get-certification — Get the attached certification info of a run"
  )

createCertificationInfo :: ParserInfo CreateCertificationArgs
createCertificationInfo = info createCertificationParser
  ( fullDesc
 <> header ( "plutus-certification-client run create-certification — Generates the certification objects of a run."
      ++ " It works only if the run succeeded and no certification was already generated before" )
  )

createCertificationParser :: Parser CreateCertificationArgs
createCertificationParser = CreateCertificationArgs
  <$> getRunParser
  <*> authParser
  <*> certificationInputParser
  -- dry-run
  <*> optional (switch
      ( long "dry-run"
      <> help "dry run"
      ))

certificationIssuerParser :: Parser CertificateIssuer
certificationIssuerParser  = CertificateIssuer
  -- name
  <$> option patternedTextReader
      ( long "issuer-name"
      <> metavar "ISSUER-NAME"
      <> help "issuer name"
      )
  -- URL
  <*> optional ( URL <$> option str
      ( long "issuer-url"
      <> metavar "ISSUER-URL"
      <> help "issuer URL"
      ))
  <*> parseSocial

parseSocial :: Parser Social
parseSocial = Social
  <$> optional ( option patternedTextReader
      ( long "twitter"
      <> metavar "TWITTER"
      <> help "twitter handle"
      ))
  <*> optional ( option patternedTextReader
      ( long "github"
      <> metavar "GITHUB"
      <> help "github handle"
      ))
  <*> option str
      ( long "contact"
      <> metavar "CONTACT"
      <> help "contact email"
      )
  <*> option patternedTextReader
      ( long "website"
      <> metavar "WEBSITE"
      <> help "website URL"
      )
  <*> optional ( option patternedTextReader
      ( long "discord"
      <> metavar "DISCORD"
      <> help "discord handle"
      ))

subjectInputParser :: Parser Subject
subjectInputParser = option patternedTextReader
  ( long "subject"
  <> metavar "SUBJECT"
  <> help "dapp subject"
  )

certificationInputParser :: Parser CertificationInput
certificationInputParser  = CertificationInput
  <$> certificationIssuerParser
  <*> option str
      ( long "summary"
      <> metavar "SUMMARY"
      <> help "dapp summary"
      )
  -- disclaimer optional
  <*> option str
      ( long "disclaimer"
      <> metavar "DISCLAIMER"
      <> help "dapp disclaimer"
      <> value Text.empty
      )
  -- TODO: add scripts
  <*> pure []



data RunCommand
  = Create !CreateRunArgs
  | Get !RunIDV1
  | Abort !AbortRunArgs
  | GetLogs !GetLogsArgs
  | GetRuns !GetRunsArgs
  | CreateCertification !CreateCertificationArgs

runCommandParser :: Parser RunCommand
runCommandParser = hsubparser
  ( command "create" (Create <$> createRunInfo)
 <> command "get" (Get <$> getRunInfo)
 <> command "abort" (Abort <$> abortRunInfo)
 <> command "get-logs" (GetLogs <$> getLogsInfo)
 <> command "get-many" (GetRuns <$> getRunsInfo)
 <> command "create-l1-certification" (CreateCertification <$> createCertificationInfo)
  )

data CreateRunArgs = CreateRunArgs !CommitOrBranch !Auth

data GetRunsArgs = GetRunsArgs !Auth !(Maybe UTCTime) !(Maybe Int)

type DeleteRun = Maybe Bool
data AbortRunArgs = AbortRunArgs !RunIDV1 !Auth !DeleteRun
type DryRun = Maybe Bool
data CreateCertificationArgs= CreateCertificationArgs !RunIDV1 !Auth !CertificationInput !DryRun

data GetLogsArgs = GetLogsArgs
  { runId :: !RunIDV1
  , after :: !(Maybe ZonedTime)
  , actionType :: !(Maybe KnownActionType)
  }

runCommandInfo :: ParserInfo RunCommand
runCommandInfo = info runCommandParser
  ( fullDesc
 <> header "plutus-certification-client run — Manage certification runs"
  )

currentProfileInfo :: ParserInfo ProfileCommand
currentProfileInfo = info currentProfileParser
  ( fullDesc
 <> header "plutus-certification-client profile Current profile Management"
  )

currentProfileParser :: Parser ProfileCommand
currentProfileParser = hsubparser
  ( command "get" (GetCurrentProfile <$> getCurrentProfileInfo)
 <> command "update" (UpdateCurrentProfile <$> updateCurrentProfileInfo)
 <> command "get-wallet-address" (GetProfileWalletAddress <$> getWalletAddressInfo)
 <> command "get-balance" (GetProfileBalance <$> getBalanceInfo)
  )

getBalanceInfo :: ParserInfo Auth
getBalanceInfo = info authParser
  ( fullDesc
  <> header "plutus-certification-client profile get-balance — Get the balance of the current profile"
  )

getWalletAddressInfo :: ParserInfo Auth
getWalletAddressInfo = info authParser
  ( fullDesc
  <> header "plutus-certification-client profile get-wallet-address — Get the wallet address of the current profile"
  )

updateCurrentProfileInfo :: ParserInfo UpdateCurrentProfileArgs
updateCurrentProfileInfo = info updateCurrentProfileInfoParser
  ( fullDesc
 <> header "plutus-certification-client profile update — Update the current profile information"
  )

updateCurrentProfileInfoParser :: Parser UpdateCurrentProfileArgs
updateCurrentProfileInfoParser = UpdateCurrentProfileArgs
  <$> authParser
  <*> profileBodyParser

dappParser :: Parser DApp
dappParser = DApp (toId 0)
  <$> option str
        ( long "dapp-name"
       <> metavar "DAPP_NAME"
       <> help "dapp name"
        )
  <*> option str
        ( long "dapp-owner"
       <> metavar "DAPP_GITHUB_OWNER"
       <> help "dapp github owner"
        )
  <*> option str
        ( long "dapp-repo"
       <> metavar "DAPP_GITHUB_REPO"
       <> help "dapp github repo"
        )
  <*> option str
        ( long "dapp-version"
       <> metavar "DAPP_VERSION"
       <> help "dapp version"
        )
  <*> optional gitHubAccessTokenParser
  <*> optional subjectInputParser

profileBodyParser :: Parser ProfileBody
profileBodyParser = ProfileBody
  <$> profileParser
  <*> optional dappParser

profileParser :: Parser Profile
profileParser = Profile (toId 0) (error "this won't be evaluated")
  <$> optional (option patternedTextReader
        ( long "website"
       <> metavar "WEBSITE"
       <> help "dapp website url"
        ))
  <*> optional (option patternedTextReader
        ( long "twitter"
       <> metavar "TWITTER"
       <> help "twitter account"
        ))
  <*> optional (option patternedTextReader
        ( long "linkedin"
       <> metavar "LINKEDIN"
       <> help "linkedin account"
        ))
  <*> optional (option patternedTextReader
        ( long "email"
       <> metavar "EMAIL"
       <> help "email"
        ))
  <*> optional (option patternedTextReader
        ( long "contact-email"
       <> metavar "CONTACT_EMAIL"
       <> help "contact email"
        ))
  <*> optional (option str
        ( long "company-name"
       <> metavar "COMPANY_NAME"
       <> help "company name"
        ))
  <*> optional (option str
        ( long "full-name"
       <> metavar "FULL_NAME"
       <> help "full name"
        ))

patternedTextReader :: (KnownSymbol n,KnownSymbol p) => ReadM (PatternedText n p)
patternedTextReader = do
  v <- str
  case mkPatternedText (Text.pack v) of
    Right p -> pure p
    Left e -> fail (show e)


getCurrentProfileInfo :: ParserInfo Auth
getCurrentProfileInfo = info authParser
  ( fullDesc
 <> header "plutus-certification-client profile get — Get the current profile information"
  )

versionCommandInfo :: ParserInfo ()
versionCommandInfo = info (pure ())
  ( fullDesc
 <> header "plutus-certification-client version — Get the version of the server"
  )

walletAddressCommandInfo :: ParserInfo ()
walletAddressCommandInfo = info (pure ())
  ( fullDesc
 <> header "plutus-certification-client wallet-address — Get the wallet address the backend operates with"
  )

loginInfo :: ParserInfo LoginBody
loginInfo = info loginBodyParser
  ( fullDesc
 <> header "plutus-certification-client login — Login to the backend"
  )

loginBodyParser :: Parser LoginBody
loginBodyParser = LoginBody
  <$> option str
        ( long "wallet-address"
       <> metavar "WALLET_ADDRESS"
       <> help "bech32 wallet address"
        )
  <*> option hexaReader
        ( long "wallet-public-key"
       <> metavar "WALLET_PUBLIC_KEY"
       <> help "hex-encoded public key"
        )
  <*> option hexaReader
        ( long "message-signature"
       <> metavar "MESSAGE_SIGNATURE"
       <> help "encoded signature of the message"
        )
  <*> optional (option generalReader
        ( long "expiration"
       <> metavar "EXPIRATION"
       <> help "lifetime of the token in seconds. if bigger then server's max, server's max will be used"
        ))
serverTimestampInfo :: ParserInfo ()
serverTimestampInfo = info (pure ())
  ( fullDesc
 <> header "plutus-certification-client server-timestamp — Get the server timestamp"
  )

data Command
  = CmdRun !RunCommand
  | CmdVersion
  | CmdWalletAddress
  | CmdCurrentProfile !ProfileCommand
  | CmdGetRepositoryInfo !GetRepositoryInfoArgs
  | CmdLogin !LoginBody
  | CmdServerTimestamp
  | CmdGetAdaUsdPrice
  | CmdGetAllTiers
  | CmdSubscriptions !SubscriptionCommand

data SubscriptionCommand
  = GetSubscriptions GetSubscriptionsArgs
  | Subscribe SubscribeArgs
  | CancelPendingSubscriptions !Auth
  | GetActiveFeatures !Auth

data GetSubscriptionsArgs = GetSubscriptionsArgs !Auth !Bool

data SubscribeArgs = SubscribeArgs !Auth !Int64

data GetRepositoryInfoArgs = GetGitHubAddressArgs
  { owner :: !Text
  , repo :: !Text
  , gitHubAccessToken :: !(Maybe ApiGitHubAccessToken)
  }

data ProfileCommand
    = GetCurrentProfile !Auth
    | UpdateCurrentProfile !UpdateCurrentProfileArgs
    | GetProfileWalletAddress !Auth
    | GetProfileBalance !Auth

data UpdateCurrentProfileArgs = UpdateCurrentProfileArgs !Auth !ProfileBody

commandParser :: Parser Command
commandParser = hsubparser
  ( command "run" (CmdRun <$> runCommandInfo)
 <> command "version" (CmdVersion <$ versionCommandInfo)
 <> command "profile" (CmdCurrentProfile <$> currentProfileInfo)
 <> command "wallet-address" (CmdWalletAddress <$ walletAddressCommandInfo)
 <> command "get-repo-info" (CmdGetRepositoryInfo <$> getGitHubRepoInfo)
 <> command "login" (CmdLogin <$> loginInfo)
 <> command "server-timestamp" (CmdServerTimestamp <$ serverTimestampInfo)
 <> command "get-ada-usd-price" (CmdGetAdaUsdPrice <$ getAdaUsdPriceInfo)
 <> command "get-tiers" (CmdGetAllTiers <$ getAllTiersInfo)
 <> command "subscriptions" (CmdSubscriptions <$> subscriptionsCommandInfo)
  )

subscriptionsCommandInfo :: ParserInfo SubscriptionCommand
subscriptionsCommandInfo = info subscriptionsCommandParser
  ( fullDesc
 <> header "plutus-certification-client subscriptions — Manage subscriptions"
  )

subscriptionsCommandParser :: Parser SubscriptionCommand
subscriptionsCommandParser = hsubparser
  ( command "get" (GetSubscriptions <$> getSubscriptionsInfo)
 <> command "choose" (Subscribe <$> subscribeInfo)
 <> command "cancel-pending" (CancelPendingSubscriptions <$> cancelPendingSubscriptionInfo)
 <> command "get-active-features" (GetActiveFeatures <$> getActiveFeaturesInfo)
  )

getActiveFeaturesInfo :: ParserInfo Auth
getActiveFeaturesInfo = info authParser
  ( fullDesc
 <> header "plutus-certification-client subscriptions get-active-features — Get all active features"
  )
cancelPendingSubscriptionInfo :: ParserInfo Auth
cancelPendingSubscriptionInfo = info authParser
  ( fullDesc
 <> header "plutus-certification-client subscriptions cancel-pending — Cancel all pending subscriptions"
  )

subscribeInfo :: ParserInfo SubscribeArgs
subscribeInfo = info subscribeParser
  ( fullDesc
  <> header "plutus-certification-client subscriptions choose — Subscribe to a tier"
  )

subscribeParser :: Parser SubscribeArgs
subscribeParser = SubscribeArgs
  <$> authParser
  <*> option auto
        ( long "tier-id"
       <> metavar "TIER-ID"
       <> help "Tier to subscribe to"
        )

getSubscriptionsInfo :: ParserInfo GetSubscriptionsArgs
getSubscriptionsInfo = info getSubscriptionsParser
  ( fullDesc
  <> header "plutus-certification-client subscriptions get — Get the current/all subscriptions"
  )

getSubscriptionsParser :: Parser GetSubscriptionsArgs
getSubscriptionsParser = GetSubscriptionsArgs
  <$> authParser
  <*> switch
        ( long "all"
       <> help "Get all subscriptions, not only the active ones"
        )

getAdaUsdPriceInfo :: ParserInfo ()
getAdaUsdPriceInfo = info (pure ())
  ( fullDesc
 <> header "plutus-certification-client get-ada-usd-price — Get the current ADA/USD price"
  )

getAllTiersInfo :: ParserInfo ()
getAllTiersInfo = info (pure ())
  ( fullDesc
 <> header "plutus-certification-client get-tiers — Get the available tiers"
  )

getGitHubRepoInfo :: ParserInfo GetRepositoryInfoArgs
getGitHubRepoInfo = info getGitHubRepoParser
  ( fullDesc
 <> header "plutus-certification-client get-github-repo — Get the github repo information"
  )

getGitHubRepoParser :: Parser GetRepositoryInfoArgs
getGitHubRepoParser = GetGitHubAddressArgs
  <$> option str
        ( long "owner"
       <> metavar "GITHUB_OWNER"
       <> help "github owner"
        )
  <*> option str
        ( long "repo"
       <> metavar "GITHUB_REPO"
       <> help "github repo"
        )
  <*> optional (ApiGitHubAccessToken <$> gitHubAccessTokenParser)

data Args = Args
  { certificationURL :: !BaseUrl
  , cmd :: !Command
  }

baseUrlReader :: ReadM BaseUrl
baseUrlReader = do
  urlStr <- str
  case parseBaseUrl urlStr of
    Left e -> case fromException e of
      Just (InvalidBaseUrlException s) -> readerError $ "invalid URL '" ++ urlStr ++ "': " ++ s
      Nothing -> readerError $ "exception parsing '" ++ urlStr ++ "' as a URL: " ++ displayException e
    Right b -> pure b

hexaReader :: ReadM ByteString
hexaReader = do
  urlStr <- str
  case Hexa.decode (BS.pack urlStr) of
    Left e -> readerError $ "invalid hexa string '" ++ urlStr ++ "': " ++ e
    Right b -> pure b

argsParser :: Parser Args
argsParser = Args
  <$> option baseUrlReader
        ( long "certification-url"
       <> metavar "CERTIFICATION_URL"
       <> help "URL of the certification server"
       <> showDefaultWith showBaseUrl
       <> value (BaseUrl Https "testing.dapps.iog.io" 443 "")
        )
  <*> commandParser

argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
 <> header "plutus-certification-cli — A tool for interacting with the Plutus Certification service"
  )

addAuth :: PublicKey -> AuthenticatedRequest (AuthProtect "public-key")
addAuth = flip mkAuthenticatedRequest (addHeader hAuthorization . BS.unpack . unPublicKey)

addJwtAuth :: JWT -> AuthenticatedRequest (AuthProtect "jwt-token")
addJwtAuth = flip mkAuthenticatedRequest (addHeader hAuthorization . ("Bearer " <>) . BS.unpack . unJWT)

type instance AuthClientData (AuthProtect "public-key") = PublicKey
type instance AuthClientData (AuthProtect "jwt-token") = JWT

main :: IO ()
main = do
  args <- execParser argsInfo
  manager <- newTlsManagerWith $ tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  let apiClient = client $ Proxy @(API "public-key")
      cEnv = mkClientEnv manager args.certificationURL
      handle :: (ToJSON a) => ClientM a -> IO ()
      handle c = runClientM c cEnv >>= either throwIO (LBS.putStrLn . encode)
      withAuth :: ToJSON b
               => Auth
               -> (forall a. Client ClientM (API a) -> AuthenticatedRequest (AuthProtect a) -> ClientM b)
               -> IO ()
      withAuth auth f = handle $ case auth of
        PublicKeyAuth pubKey -> f apiClient (addAuth pubKey)
        JWTAuth jwt -> f (client $ Proxy @(API "jwt-token")) (addJwtAuth jwt)
  case args.cmd of
    CmdVersion ->
      handle $ apiClient.version
    CmdWalletAddress ->
      handle $ apiClient.walletAddress
    CmdRun (Create (CreateRunArgs ref auth)) ->
      withAuth auth $ \c authKey -> c.createRun authKey ref
    CmdRun (Get ref) ->
      handle $ apiClient.getRun ref
    CmdRun (Abort (AbortRunArgs ref auth deleteRun)) ->
      withAuth auth $ \c authKey -> True <$ c.abortRun authKey ref deleteRun
    --TODO: investigate why ZonedTime doesn't serialize properly
    CmdRun (GetLogs (GetLogsArgs ref zt act)) ->
      handle $ apiClient.getLogs ref zt act
    CmdRun (GetRuns (GetRunsArgs pubKey after' count')) ->
      withAuth pubKey $ \c authKey -> c.getRuns authKey after' count'
    CmdGetRepositoryInfo (GetGitHubAddressArgs owner' repo' gitHubAccessToken') ->
      handle $ apiClient.getRepositoryInfo owner' repo' gitHubAccessToken'
    CmdRun (CreateCertification (CreateCertificationArgs ref auth certInput dryRun)) ->
      withAuth auth $ \c authKey ->
        c.createCertification authKey ref certInput dryRun
    CmdCurrentProfile (GetCurrentProfile auth) ->
      withAuth auth $ \c authKey -> c.getCurrentProfile authKey
    CmdCurrentProfile (UpdateCurrentProfile (UpdateCurrentProfileArgs auth profileBody)) -> do
      withAuth auth $ \c authKey -> c.updateCurrentProfile authKey profileBody
    CmdCurrentProfile (GetProfileWalletAddress auth) ->
      withAuth auth $ \c authKey -> c.getProfileWalletAddress authKey
    CmdCurrentProfile (GetProfileBalance auth) ->
      withAuth auth $ \c authKey -> c.getProfileBalance authKey
    CmdLogin loginBody -> do
      handle $ apiClient.login loginBody
    CmdServerTimestamp ->
      handle $ apiClient.serverTimestamp
    CmdGetAdaUsdPrice ->
      handle $ apiClient.getAdaUsdPrice
    CmdGetAllTiers ->
      handle $ apiClient.getAllTiers
    CmdSubscriptions (GetSubscriptions (GetSubscriptionsArgs auth all')) ->
      withAuth auth $ \c authKey -> c.getProfileSubscriptions authKey (Just (not all'))
    CmdSubscriptions (Subscribe (SubscribeArgs auth tierId')) ->
      withAuth auth $ \c authKey -> c.subscribe authKey tierId'
    CmdSubscriptions (CancelPendingSubscriptions auth) ->
      withAuth auth $ \c authKey -> c.cancelPendingSubscriptions authKey
    CmdSubscriptions (GetActiveFeatures auth) ->
      withAuth auth $ \c authKey -> c.getActiveFeatures authKey
