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
import Control.Exception hiding (Handler)
import Data.Aeson
import Data.ByteString.Char8 as BS hiding (hPutStrLn,foldl')
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
import Observe.Event.Servant.Client
import System.IO
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Network.Wai
import Plutus.Certification.API
import Plutus.Certification.Cache hiding (lookup)
import Plutus.Certification.Cicero
import Plutus.Certification.Client
import Plutus.Certification.Server as Server
import Plutus.Certification.Local
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

import Paths_plutus_certification qualified as Package
import IOHK.Certification.Persistence qualified as DB
import IOHK.Certification.Persistence (toId)

import qualified Data.ByteString.Lazy.Char8 as LBS

data Backend
  = Local
  | Cicero !BaseUrl

data Args = Args
  { port :: !Port
  , host :: !HostPreference
  , backend :: !Backend
  , wallet :: !WalletArgs
  , githubToken :: !(Maybe GitHubAccessToken)
  , auth :: !AuthMode
  , signatureTimeout :: !Seconds
  }

baseUrlReader :: ReadM BaseUrl
baseUrlReader = do
  urlStr <- str
  case parseBaseUrl urlStr of
    Left e -> case fromException e of
      Just (InvalidBaseUrlException s) -> readerError $ "invalid URL '" ++ urlStr ++ "': " ++ s
      Nothing -> readerError $ "exception parsing '" ++ urlStr ++ "' as a URL: " ++ displayException e
    Right b -> pure b

ciceroParser :: Parser Backend
ciceroParser = Cicero
  <$> option baseUrlReader
      ( long "cicero-url"
     <> metavar "CICERO_URL"
     <> help "URL of the cicero server"
     <> showDefaultWith showBaseUrl
     <> Opts.value ( BaseUrl Http "localhost" 8080 "")
      )

localParser :: Parser Backend
localParser = flag' Local
  ( long "local"
 <> help "Run with the local in-process \"job scheduler\""
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
  <*> (localParser <|> ciceroParser)
  <*> walletParser
  <*> optional gitHubAccessTokenParser
  <*> (jwtArgsParser <|> plainAddressAuthParser)
  <*> option auto
      ( long "signature-timeout"
     <> metavar "SIGNATURE_TIMEOUT"
     <> help "specifies the maximum amount of time allowed for the signing the login message, expressed in seconds"
     <> showDefault
     <> Opts.value 60
      )

data AuthMode = JWTAuth JWTArgs | PlainAddressAuth

plainAddressAuthParser :: Parser AuthMode
plainAddressAuthParser = flag' PlainAddressAuth
  ( long "unsafe-plain-address-auth"
 <> help "use plain address authentication - NOTE: this is for testing only, and should not be used in production"
  )
defaultJWTExpiration :: Integer
defaultJWTExpiration = 60 * 60 * 24 * 30 -- 30 days

jwtArgsParser :: Parser AuthMode
jwtArgsParser =  JWTAuth <$> (JWTArgs
  <$> option str
      ( long "jwt-secret"
     <> metavar "JWT_SECRET"
     <> help "the jwt secret key"
      )
  <*> option auto
      ( long "jwt-expiration-seconds"
      <> metavar "JWT_EXPIRATION"
      <> help "the jwt expiration time in seconds"
      <> showDefault
      <> Opts.value defaultJWTExpiration
      ))
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

data InitializingField
  = ArgsField Args
  | VersionField Version

data RootEventSelector f where
  Initializing :: RootEventSelector InitializingField
  OnException :: RootEventSelector OnExceptionField
  InjectServerSel :: forall f . !(ServerEventSelector f) -> RootEventSelector f
  InjectCrashing :: forall f . !(Crashing f) -> RootEventSelector f
  InjectRunRequest :: forall f . !(RunRequest f) -> RootEventSelector f
  InjectServeRequest :: forall f . !(ServeRequest f) -> RootEventSelector f
  InjectRunClient :: forall f . !(RunClientSelector f) -> RootEventSelector f
  InjectLocal :: forall f . !(LocalSelector f) -> RootEventSelector f
  InjectSynchronizer :: forall f . !(SynchronizerSelector f) -> RootEventSelector f

renderRoot :: RenderSelectorJSON RootEventSelector
renderRoot Initializing =
  ( "initializing"
  , \case
      ArgsField args -> ("args", object [ "port" .= args.port
                                        , "host" .= show args.host
                                        , "backend" .= case args.backend of
                                            Cicero u -> object [ "cicero-url" .= u ]
                                            Local -> String "local"
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
renderRoot (InjectRunRequest s) = runRequestJSON s
renderRoot (InjectServeRequest s) = renderServeRequest s
renderRoot (InjectRunClient s) = renderRunClientSelector s
renderRoot (InjectLocal s) = renderLocalSelector s
renderRoot (InjectSynchronizer s) = renderSynchronizerSelector s

-- | plain address authentication
-- NOTE: this is for testing only, and should not be used in production
plainAddressAuthHandler :: AuthHandler Request (DB.ProfileId,UserAddress)
plainAddressAuthHandler = mkAuthHandler handler
  where
  handler :: (MonadError ServerError m,MonadIO m,MonadMask m)
          => Request
          -> m (DB.ProfileId, UserAddress)
  handler req = either throw401 ensureProfile $ do
    maybeToEither "Missing Authorization header" $ lookup "Authorization" $ requestHeaders req
  maybeToEither e = maybe (Left e) Right

-- | JWT authentication
jwtAuthHandler :: String -> AuthHandler Request (DB.ProfileId,UserAddress)
jwtAuthHandler secret = mkAuthHandler handler
  where
  handler :: (MonadError ServerError m,MonadIO m,MonadMask m)
          => Request
          -> m (DB.ProfileId, UserAddress)
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
        Right ((pid,addr),expiration) -> do
          -- verify expiration
          now <- liftIO getCurrentTime
          -- compare the expiration time with the current time
          when (now > expiration) $ throw401 "JWT token expired"
          pure (toId pid, UserAddress addr)
    Nothing -> throw401 "Missing Authorization header"
  extractToken =
    let isSpace = (== ' ')
    in BS.dropWhileEnd isSpace . BS.dropWhile isSpace . snd . BS.break isSpace

throw401 :: MonadError ServerError m => LBS.ByteString -> m a
throw401 msg = throwError $ err401 { errBody = msg }

genAuthServerContext :: AuthMode
                     -> Context (AuthHandler Request (DB.ProfileId,UserAddress) ': '[])
genAuthServerContext mSecret = (case mSecret of
  PlainAddressAuth -> plainAddressAuthHandler
  JWTAuth JWTArgs{..} -> jwtAuthHandler jwtSecret ) :. EmptyContext

initDb :: IO ()
initDb = void $ try' $ DB.withDb DB.createTables
  where
  try' :: IO a -> IO (Either SomeException a)
  try' = try

main :: IO ()
main = do
  args <- execParser opts
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
      caps <- case args.backend of
        Cicero ciceroUrl -> do
          actionCache <- newCacheMapIO

          clientCaps <- clientCapsIO
            ciceroUrl
            (hoistScheduleCrash liftIO scheduleCrash)
            ( hoistEventBackend liftIO
            . narrowEventBackend InjectRunRequest
            $ eb
            )
          pure . ciceroServerCaps ( hoistEventBackend liftIO
                                  . narrowEventBackend InjectRunClient
                                  $ eb
                                  ) $ CiceroCaps {..}
        Local -> hoistServerCaps liftIO <$> localServerCaps ( narrowEventBackend InjectLocal eb )
      let settings = defaultSettings
                   & setPort args.port
                   & setHost args.host
                   & setOnException (\r e -> when (defaultShouldDisplayException e) $ withEvent eb OnException \ev -> do
                                        addField ev $ OnExceptionField r e
                                        schedule scheduleCrash (setAncestor $ reference ev))
                   & setInstallShutdownHandler (putMVar closeSocketVar)
                   & setBeforeMainLoop (finalize initEv)
          corsPolicy = simpleCorsResourcePolicy
                     { corsRequestHeaders = ["Content-Type", "Authorization"]
                     , corsMethods = [methodGet,methodPost,methodPut,methodDelete,methodHead]
                     }
      _ <- initDb
      _ <- forkIO $ startTransactionsMonitor (narrowEventBackend InjectSynchronizer eb) (args.wallet) 10
      -- TODO: this has to be refactored
      runSettings settings . application (narrowEventBackend InjectServeRequest eb) $
        cors (const $ Just corsPolicy) .
        serveWithContext (Proxy @APIWithSwagger) (genAuthServerContext args.auth) .
        (\r -> swaggerSchemaUIServer (documentation args.auth) :<|> server (serverArgs args caps r eb))
  exitFailure
  where
  serverArgs args caps r eb = ServerArgs
    caps (args.wallet) args.githubToken (jwtArgs args.auth) (be r eb) (args.signatureTimeout)
  jwtArgs PlainAddressAuth = Nothing
  jwtArgs (JWTAuth args) = Just args
  documentation PlainAddressAuth = swaggerJson
  documentation (JWTAuth _) = swaggerJsonWithLogin

  be r eb = hoistEventBackend liftIO $ narrowEventBackend InjectServerSel $ modifyEventBackend (setAncestor r) eb
