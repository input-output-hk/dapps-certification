{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad.Except
import Control.Monad.Catch hiding (try)
import Servant.Swagger.UI
import Data.Maybe
import Data.Aeson
import Data.ByteString.Char8 as BS hiding (hPutStrLn,foldl',words)
import Data.Function
import Data.Singletons
import Data.Time
import Data.Version
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Options.Applicative as Opts
import Servant
import Servant.Client.Core.BaseUrl
import System.Exit
import Observe.Event
import Servant.Server.Experimental.Auth
import Observe.Event.BackendModification
import Observe.Event.Crash
import Observe.Event.Render.JSON
import Observe.Event.Render.IO.JSON
import Observe.Event.Wai hiding (OnException)
import System.IO
import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Network.Wai
import Plutus.Certification.API
import Plutus.Certification.Server as Server
import Plutus.Certification.Server.Local
import Plutus.Certification.Server.Kube
import Data.Text (Text)
import Data.Text.Encoding
import Network.HTTP.Types.Method
import Plutus.Certification.WalletClient
import Plutus.Certification.Synchronizer
import Plutus.Certification.GitHubClient
import Control.Concurrent (forkIO)
import IOHK.Certification.Actions
import Plutus.Certification.JWT
import Data.Int
import Plutus.Certification.ProfileWallet
import Paths_plutus_certification qualified as Package
import IOHK.Certification.Persistence qualified as DB
import IOHK.Certification.Persistence.API.SQLite qualified as DB
import Data.HashSet as HashSet
import Data.Word

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import System.Environment (lookupEnv)
import Crypto.Random
import Control.Monad.Reader (ReaderT(runReaderT))

oneAda :: Word64
oneAda = 1000000

data Backend
  = Local
  | Kube !Text
  deriving (Eq, Show)

data Args = Args
  { port :: !Port
  , host :: !HostPreference
  , backend :: !Backend
  , wallet :: !WalletArgs
  , auth :: !AuthMode
  , signatureTimeout :: !Seconds
  , useWhitelist :: !Bool
  , github :: !GitHubArgs
  , bypassSubscriptionValidation :: !Bool
  , dbPath :: !FilePath
  , minAmountForAddressAssignment :: !Word64
  , adminWallet :: !(Maybe DB.ProfileWalletAddress)
  -- Normally, the admin wallet is not enforced, but only when there is no admin left
  -- though, if this is set true, will be enforced even if there are admins left
  , forceAdminAlways :: !Bool
  , vat :: DB.VatPercentage
  , invoicesFolderPath :: !FilePath
  , weasyprintPath :: !FilePath
  }

data GitHubArgs = GitHubArgs
  { accessToken :: !(Maybe GitHubAccessToken)
  , credentials :: !(Maybe GitHubCredentials)
  }

baseUrlReader :: ReadM BaseUrl
baseUrlReader = do
  urlStr <- str
  case parseBaseUrl urlStr of
    Left e -> case fromException e of
      Just (InvalidBaseUrlException s) -> readerError $ "invalid URL '" ++ urlStr ++ "': " ++ s
      Nothing -> readerError $ "exception parsing '" ++ urlStr ++ "' as a URL: " ++ displayException e
    Right b -> pure b

localParser :: Parser Backend
localParser = flag' Local
  ( long "local"
 <> help "Run with the local in-process \"job scheduler\""
  )
kubeParser :: Parser Backend
kubeParser = Kube
  <$> option str
        ( long "run-certify-image"
       <> help "The docker image (including tag) containing run-certify to use with the kubernetes-backed job scheduler"
        )

argsParser :: Parser Args
argsParser =  Args
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "server port number"
     <> showDefault
     <> Opts.value 9671
      )
  <*> option str
      ( long "bind"
     <> short 'b'
     <> metavar "HOST"
     <> help "server bind address"
     <> showDefault
     <> Opts.value "*"
      )
  <*> (localParser <|> kubeParser)
  <*> walletParser
  <*> (plainAddressAuthParser <|> jwtArgsParser)
  <*> option auto
      ( long "signature-timeout"
     <> metavar "SIGNATURE_TIMEOUT"
     <> help "specifies the maximum amount of time allowed for the signing the login message, expressed in seconds"
     <> showDefault
     <> Opts.value 60
      )
  <*> switch
      ( long "use-whitelist" <> help "use the whitelist for authentication" )
  <*> githubArgsParser

  <*> switch
      ( long "unsafe-bypass-subscription-validation"
     <> help "Bypass subscription validation"
      )
  <*> option str
      ( long "db-path"
     <> metavar "DB_PATH"
     <> help "the path to the database. If not specified, \"./certification.sqlite\" is used"
     <> showDefault
     <> Opts.value "./certification.sqlite"
      )
  <*> option auto
      ( long "min-amount-for-address-reservation"
     <> metavar "MIN_AMOUNT"
     <> help ( "the minimum amount of Lovelace required to perform an address reservation. "
            <> "CAUTION: don't use different values once one was chosen, as this will affect the previously reserved addresses too"
             )
     <> showDefault
     <> Opts.value oneAda
      )
  <*> optional (option generalReader
      ( long "admin-wallet"
     <> metavar "ADMIN_WALLET"
     <> help "the main admin wallet address"
      ))
  <*> switch
      ( long "force-admin-always"
     <> help ( "force the admin wallet to always be an admin"
            <> "NODE: When is not set the admin wallet is not enforced as an admin,"
            <> "but only when there is no admin left. Though,"
            <> "if this is set true, will be enforced even if there are other admins in the system"
             )
      )
  -- subscription VAT percentage
  <*> option auto
      ( long "subscription-vat"
     <> metavar "SUB_VAT"
     <> help "the VAT percentage to apply to the subscription price"
     <> showDefault
     <> Opts.value 0
      )
  <*> option str
      ( long "invoices-folder-path"
      <> metavar "INVOICES_FOLDER_PATH"
      <> help "the path to the folder where the invoices will be stored"
      <> showDefault
      <> Opts.value "./invoices"
      )
  <*> option str
      ( long "weasyprint-path"
      <> metavar "WEASYPRINT_PATH"
      <> help "the path to the weasyprint executable"
      <> showDefault
      <> Opts.value "weasyprint"
  )

-- | Parse a URL piece
-- NOTE: there is a duplicate in client/Main.hs
-- it is intended because we don't have yet a common library
-- which doesn't imply the full server-library
-- TODO: in the future we should split the API definition in a common library
-- and also add the common utility functions to that library
generalReader :: FromHttpApiData a => ReadM a
generalReader = do
  urlStr <- str
  case parseUrlPiece urlStr of
    Right u -> pure u
    Left t -> readerError $ Text.unpack t

data AuthMode = JWTAuth JWTArgs | PlainAddressAuth

data JWTArgsMode = JWTSecret !String | JWTGenerate
data JWTArgs = JWTArgs
  { jwtMode :: !JWTArgsMode
  , jwtExpirationSeconds :: !Integer
  }

plainAddressAuthParser :: Parser AuthMode
plainAddressAuthParser = flag' PlainAddressAuth
  ( long "unsafe-plain-address-auth"
 <> help "use plain address authentication - NOTE: this is for testing only, and should not be used in production"
  )
defaultJWTExpiration :: Integer
defaultJWTExpiration = 60 * 60 * 24 * 30 -- 30 days

-- Parse JWTMode from command line arguments
jwtModeParser :: Parser JWTArgsMode
jwtModeParser =
  ( JWTSecret <$> option str
     ( long "jwt-secret"
     <> metavar "JWT_SECRET"
     <> help "the jwt secret key"
     )
  ) <|> flag' JWTGenerate
        ( long "jwt-generate"
       <> help "use the jwt token generated within the db"
        )

jwtArgsParser :: Parser AuthMode
jwtArgsParser =  JWTAuth <$> (JWTArgs
  <$>  jwtModeParser
  <*> option auto
      ( long "jwt-expiration-seconds"
      <> metavar "JWT_EXPIRATION"
      <> help "the jwt expiration time in seconds"
      <> showDefault
      <> Opts.value defaultJWTExpiration
      ))
githubArgsParser :: Parser GitHubArgs
githubArgsParser = GitHubArgs
  <$> optional gitHubAccessTokenParser
  <*> optional (GitHubCredentials <$> gitHubClientIdParser <*> gitHubClientSecretParser)

gitHubClientIdParser :: Parser Text
gitHubClientIdParser = Text.pack <$> strOption
  ( long "github-client-id"
 <> metavar "GITHUB_CLIENT_ID"
 <> help "GitHub OAuth client ID"
  )

gitHubClientSecretParser :: Parser Text
gitHubClientSecretParser = Text.pack <$> strOption
  ( long "github-client-secret"
 <> metavar "GITHUB_CLIENT_SECRET"
 <> help "GitHub OAuth client secret"
  )

walletParser :: Parser WalletArgs
walletParser = WalletArgs
  <$> option str
      ( long "wallet-id"
     <> metavar "WALLET_ID"
     <> help "the wallet id"
      )
  <*> option str
      ( long "wallet-address"
     <> metavar "WALLET_ADDRESS"
     <> help "the wallet address"
      )
  <*> option str
      ( long "wallet-passphrase"
     <> metavar "WALLET_PASSPHRASE"
     <> help "wallet passphrase"
      )
  <*> option baseUrlReader
      ( long "wallet-url"
     <> metavar "WALLET_URL"
     <> help "URL for wallet api"
     <> showDefaultWith showBaseUrl
     <> Opts.value ( BaseUrl Http "localhost" 8090 "")
      )
  <*> option auto
      ( long "wallet-certification-price"
     <> metavar "CERTIFICATION_PRICE"
     <> help "price of certification in lovelace"
     <> showDefault
     <> Opts.value 1000000
      )

opts :: ParserInfo Args
opts = info (argsParser <**> helper)
  ( fullDesc
 <> progDesc "Run the plutus-certification server"
 <> header "plutus-certification — Certification as a service for Plutus applications"
  )

data OnAuthModeField = OnAuthModeFieldJWTSecret | OnAuthModeFieldJWTGenerate | OnAuthModeFieldPlainAddressAuth

data InitializingField
  = ArgsField Args
  | VersionField Version
data RootEventSelector f where
  Initializing :: RootEventSelector InitializingField
  OnException :: RootEventSelector OnExceptionField
  OnAuthMode :: RootEventSelector OnAuthModeField
  EnsureAdminExists :: RootEventSelector EnsureAdminExistsField
  InjectServerSel :: forall f . !(ServerEventSelector f) -> RootEventSelector f
  InjectCrashing :: forall f . !(Crashing f) -> RootEventSelector f
  InjectServeRequest :: forall f . !(ServeRequest f) -> RootEventSelector f
  InjectIOServer :: forall f . !(IOServerSelector f) -> RootEventSelector f
  InjectDbMigration :: forall f . !(DB.MigrationSelector f) -> RootEventSelector f
  InjectSynchronizer :: forall f . !(SynchronizerSelector f) -> RootEventSelector f
  MarkRunningTestsAsAborted :: RootEventSelector Int

data EnsureAdminExistsField
  = EnsureAdminExistsFieldAddress DB.ProfileWalletAddress
  | EnsureAdminExistsFieldAdmins Int

renderRoot :: RenderSelectorJSON RootEventSelector
renderRoot Initializing =
  ( "initializing"
  , \case
      ArgsField args -> ("args", object [ "port" .= args.port
                                        , "host" .= show args.host
                                        , "backend" .= case args.backend of
                                            Local -> String "local"
                                            Kube img -> object [ "kube" .= String img ]
                                        ])
      VersionField v -> ("version", toJSON $ versionBranch v)
  )
renderRoot OnException =
  ( "handling-exception"
  , renderOnExceptionField renderJSONException
  )
renderRoot (InjectServerSel serverSel) =
  renderServerEventSelector serverSel
renderRoot (InjectCrashing s) = renderCrashing s
renderRoot (InjectServeRequest s) = renderServeRequest s
renderRoot (InjectIOServer s) = renderIOServerSelector s
renderRoot (InjectDbMigration s) = DB.renderMigrationSelector s
renderRoot (InjectSynchronizer s) = renderSynchronizerSelector s
renderRoot OnAuthMode =
  ( "auth-mode"
  , \case
      OnAuthModeFieldJWTSecret -> ("mode","jwt-secret")
      OnAuthModeFieldJWTGenerate -> ("mode","jwt-generate-secret")
      OnAuthModeFieldPlainAddressAuth -> ("mode","plain-address-auth")
  )
renderRoot MarkRunningTestsAsAborted =
  ( "mark-running-tests-as-aborted"
  , ("count" .=)
  )
renderRoot EnsureAdminExists =
  ( "ensure-admin-exists", \case
      EnsureAdminExistsFieldAddress addr -> ("address", toJSON addr)
      EnsureAdminExistsFieldAdmins n -> ("admins", toJSON n)
  )

-- | plain address authentication
-- NOTE: this is for testing only, and should not be used in production
plainAddressAuthHandler :: WithDB -> Maybe Whitelist -> AuthHandler Request (DB.ProfileId,ProfileWalletAddress)
plainAddressAuthHandler withDb whitelist = mkAuthHandler handler
  where
  handler :: (MonadError ServerError m,MonadIO m,MonadMask m)
          => Request
          -> m (DB.ProfileId, ProfileWalletAddress)
  handler req = do
     bs <- either throw401 pure $ extractAddress req
     verifyWhiteList whitelist (decodeUtf8 bs)
     runReaderT (ensureProfileFromBs bs) (WithDBWrapper withDb)
  maybeToEither e = maybe (Left e) Right
  extractAddress = maybeToEither "Missing Authorization header" . lookup "Authorization" . requestHeaders

-- | JWT authentication
jwtAuthHandler :: Maybe Whitelist
               -> String
               -> AuthHandler Request (DB.ProfileId,ProfileWalletAddress)
jwtAuthHandler whitelist secret = mkAuthHandler handler
  where
  handler :: (MonadError ServerError m,MonadIO m,MonadMask m)
          => Request
          -> m (DB.ProfileId, ProfileWalletAddress)
  handler req = case lookup "Authorization" $ requestHeaders req of
    Just authHeader -> do
      let jwtToken = extractToken authHeader
      unless (jwtToken /= BS.empty ) $ throw401 "Missing JWT token"
      -- decode the token
      case jwtDecode @(Int64,Text) secret (decodeUtf8 jwtToken) of
        Left err -> throw401 $ LBS.fromStrict $ BS.pack $  case err of
            JWTDecodingFailure -> "JWT decoding failure"
            JWTSigVerificationFailure -> "JWT signature verification failure"
            JWTClaimsVerificationFailure -> "JWT claims verification failure"
            JWTDefaultKeyDecodingFailure err' -> "JWT default key decoding failure " <> err'
            JWTExpirationMissing -> "JWT expiration missing"

        Right ((pid,textAddress),expiration)
          | Right addr <- DB.mkPatternedText textAddress -> do
            -- verify expiration
            now <- liftIO getCurrentTime
            -- verify if the address is whitelisted
            verifyWhiteList whitelist textAddress
            -- compare the expiration time with the current time
            when (now > expiration) $ throw401 "JWT token expired"
            pure (toId pid, addr)
          | otherwise -> throw401 "Invalid address"

    Nothing -> throw401 "Missing Authorization header"
  extractToken =
    let isSpace = (== ' ')
    in BS.dropWhileEnd isSpace . BS.dropWhile isSpace . snd . BS.break isSpace

-- | Get the whitelisted addresses from $WLIST env var
whitelisted :: IO Whitelist
whitelisted = do
  HashSet.fromList . fmap Text.pack . words . fromMaybe [] <$> lookupEnv "WLIST"

throw401 :: MonadError ServerError m => LBS.ByteString -> m a
throw401 msg = throwError $ err401 { errBody = msg }

genAuthServerContext :: WithDB
                     -> Maybe Whitelist
                     -> Maybe JWTConfig
                     -> Context (AuthHandler Request (DB.ProfileId,ProfileWalletAddress) ': '[])
genAuthServerContext withDb whitelist mSecret = (case mSecret of
  Nothing -> plainAddressAuthHandler withDb whitelist
  Just JWTConfig{..} -> jwtAuthHandler whitelist jwtSecret ) :. EmptyContext

initDb :: EventBackend IO r RootEventSelector -> WithDB -> Bool -> IO ()
initDb eb withDb isEmpty = withDb do
    let eb'= hoistEventBackend liftIO $ narrowEventBackend InjectDbMigration eb
    DB.ensureTables eb' isEmpty
    when isEmpty DB.addInitialData

verifyTheAdmin :: EventBackend IO r RootEventSelector -> WithDB -> Args -> IO ()
verifyTheAdmin eb withDb Args{..}
  | Nothing <- adminWallet = pure ()
  | Just adminWallet' <- adminWallet = withEvent eb EnsureAdminExists \ev -> do
    pidM <- runReaderT (ensureProfile adminWallet') (WithDBWrapper withDb)
    -- throw error if the profile wasn't created or did
    unless (isJust pidM) $ error "The admin wallet is not in the db and couldn't be enforced"
    addField ev $ EnsureAdminExistsFieldAddress adminWallet'
    -- if no admin enforce the provided one
    allAdmins <- withDb $ DB.ensureAdminExists forceAdminAlways adminWallet'
    addField ev $ EnsureAdminExistsFieldAdmins allAdmins

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  args :: Args <- execParser opts
  eb <- jsonHandleBackend stderr renderJSONException renderRoot
  withEvent eb Initializing \initEv -> do
    addField initEv $ VersionField Package.version
    addField initEv $ ArgsField args

    crashVar <- newEmptyMVar
    closeSocketVar <- newEmptyMVar
    let doCrash = void $ tryPutMVar crashVar ()
        waitForCrash = do
          (closeSocket, ()) <- concurrently (takeMVar closeSocketVar) (takeMVar crashVar)
          closeSocket
    withAsync waitForCrash \_ -> withScheduleCrash (narrowEventBackend InjectCrashing eb) doCrash \scheduleCrash -> do
      caps <- hoistServerCaps liftIO <$> (case args.backend of
        Local -> localServerCaps
        Kube img -> flip kubeServerCaps img) ( narrowEventBackend InjectIOServer eb )
      let settings = defaultSettings
                   & setPort args.port
                   & setHost args.host
                   & setOnException (\r e -> when (defaultShouldDisplayException e) $ withEvent eb OnException \ev -> do
                                        addField ev $ OnExceptionField r e
                                        -- if the exception is a validation exception, we don't want to crash the server
                                        let sqlValidationEx = fromException @DB.SqlDataValidationException e
                                        unless (isJust sqlValidationEx) $
                                          schedule scheduleCrash (setAncestor $ reference ev)
                                    )
                   & setInstallShutdownHandler (putMVar closeSocketVar)
                   & setBeforeMainLoop (finalize initEv)
          corsPolicy = simpleCorsResourcePolicy
                     { corsRequestHeaders = ["Content-Type", "Authorization"]
                     , corsMethods = [methodGet,methodPost,methodPut,methodDelete,methodHead]
                     }
      -- get the whitelisted addresses from $WLIST env var
      -- if useWhitelist is set to false the whitelist is ignored
      whitelist <- if not args.useWhitelist then pure Nothing else Just <$> whitelisted

      -- initialize the address rotation
      addressRotation <- liftIO $ newMVar emptyAddressRotation

      -- open the db connection
      conn <- DB.sqliteOpen (args.dbPath)

      -- check if the db is empty
      isEmpty <- Prelude.null <$> DB.withSQLiteConnection conn DB.sqlLiteGetAllTables

      -- initialize the db and subsequent migrations if needed
      _ <- initDb eb (DB.withConnection conn) isEmpty
      -- verify if the admin wallet is in the db and enforce it
      verifyTheAdmin eb (DB.withConnection conn) args

      -- if is local mark all previous running tests as aborted
      when (args.backend == Local) $
        markAllRunningAsAborted eb conn
      jwtConfig <- getJwtArgs conn eb args
      adaPriceRef <- startSynchronizer conn eb scheduleCrash args
      -- TODO: this has to be refactored
      runSettings settings . application (narrowEventBackend InjectServeRequest eb) $
        cors (const $ Just corsPolicy) .
        serveWithContext (Proxy @APIWithSwagger) (genAuthServerContext (DB.withConnection conn) whitelist jwtConfig) .
        (\r -> swaggerSchemaUIServer (documentation args.auth) :<|> server (
            serverArgs conn args caps r eb whitelist adaPriceRef jwtConfig
            addressRotation args.vat
        ))
  exitFailure
  where
  markAllRunningAsAborted eb conn = withEvent eb MarkRunningTestsAsAborted \ev -> do
      now <- getCurrentTime
      len <- DB.withConnection conn $ DB.markAllRunningAsAborted now
      addField ev len

  getJwtArgs conn eb args = withEvent eb OnAuthMode \ev ->
    case (args.auth) of
      JWTAuth (JWTArgs mode expiration) -> Just <$> do
        secret <- case mode of
            JWTSecret secret -> do
              addField ev OnAuthModeFieldJWTSecret
              pure secret
            JWTGenerate -> do
              addField ev OnAuthModeFieldJWTGenerate
              getJWTSecretFromDB conn
        pure $ JWTConfig secret expiration
      _ -> do
        addField ev OnAuthModeFieldPlainAddressAuth
        pure Nothing

  --getJWTSecretFromDB :: Args -> IO String
  getJWTSecretFromDB conn = do
    -- check if the secret is already in the db
    maybeSecret <- DB.withConnection conn DB.getJWTSecret
    case maybeSecret of
      -- if not generate a new one and store it in the db
      Nothing -> do
        secret <- generateSecret
        DB.withConnection conn  (DB.insertJWTSecret (Text.pack secret))
        pure secret
      -- if yes return it
      Just secret -> pure (Text.unpack secret)
    where
    generateSecret :: IO String
    generateSecret = do
      g <- getSystemDRG
      let (randomBytes,_) = randomBytesGenerate 30 g
          randomText = BS.unpack randomBytes
      pure randomText

  startSynchronizer conn eb scheduleCrash args = do
    ref <- newIORef Nothing
    _ <- forkIO $ startTransactionsMonitor
            (narrowEventBackend InjectSynchronizer eb) scheduleCrash
            args.wallet ref 10 (args.minAmountForAddressAssignment)
            args.vat $  SynchronizerEnv (DB.withConnection conn) args.invoicesFolderPath args.weasyprintPath
    pure ref
  serverArgs conn  args serverCaps r eb whitelist ref serverJWTConfig serverAddressRotation serverVat = ServerArgs
    { serverWalletArgs  = args.wallet
    , serverGithubToken = args.github.accessToken
    , serverEventBackend = be r eb
    , serverSigningTimeout = args.signatureTimeout
    , serverWhitelist = whitelist
    , serverValidateSubscriptions = not args.bypassSubscriptionValidation
    , serverGitHubCredentials = args.github.credentials
    , serverAdaUsdPrice = liftIO $ readIORef ref
    , withDb = DB.withSQLiteConnection conn
    , serverInvoicesFolder = args.invoicesFolderPath
    , ..
    }
  documentation PlainAddressAuth = swaggerJson
  documentation (JWTAuth _) = swaggerJsonWithLogin

  be r eb = hoistEventBackend liftIO $ narrowEventBackend InjectServerSel $ modifyEventBackend (setAncestor r) eb
