{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Certification.Metadata.Types where

import           Control.Lens         hiding (index, (.=))
import           Data.Aeson
import           GHC.Generics (Generic)
import           Data.Swagger         hiding (Contact,summary)

import           Data.Proxy

import           Data.Text as Text
import           Data.Text.Encoding as Text

--import           Control.Exception ( throw)
import           Data.Vector
import           Plutus.Certification.Internal
import           IOHK.Certification.SignatureVerification
import           Network.URI
import           Data.ByteString (ByteString)
import           Control.Monad (when)

import qualified IOHK.Certification.Persistence as DB
import qualified Data.Swagger.Lens as SL
import qualified Data.Aeson.KeyMap as KM
import GHC.OverloadedLabels
import IOHK.Certification.Persistence (CertificationLevel)

--------------------------------------------------------------------------------
-- | ON-CHAIN METADATA

newtype MetadataUrl = MetadataUrl { unMetadata :: URI }
  deriving (Generic,Show)

instance ToJSON MetadataUrl where
  -- Metadata will serialize to a string if it's less than 64 bytes
  -- otherwise it will serialize to an array of strings max 64 bytes each
  -- split64 function will be used to split the string into an array of strings
  toJSON (MetadataUrl b) = toJSON $ split64 $ Text.pack $ show b

instance FromJSON MetadataUrl where
  -- first try to parse it as a string
  parseJSON (String s) = MetadataUrl <$> parseMetadataURI s
  -- if that fails, try to parse it as an array of strings
  parseJSON (Array a) = do
    -- concat all the strings in the array
    -- into a single string
    let urlText = Text.concat $ toList $ fmap (\case
          String s -> s
          _ -> error "Invalid metadata"
          ) a
    MetadataUrl <$> parseMetadataURI urlText

  parseJSON _ = fail "Invalid metadata"

parseMetadataURI :: MonadFail m => Text -> m URI
parseMetadataURI urlText =
  case parseURI $ Text.unpack urlText of
    Nothing -> fail $ "Invalid URL: " <> Text.unpack urlText
    Just b | validURLSchema $ uriScheme b -> pure b
    Just b -> fail $ "Invalid URL schema " <> uriScheme b

validURLSchema :: String -> Bool
validURLSchema str = case str of
  "ipfs://" -> True
  "https:" -> True
  "http:" -> True
  _ -> False

instance ToSchema MetadataUrl where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "MetadataUrl") $ mempty
      & type_ ?~ SwaggerString
      & SL.pattern ?~ "^(https:|http:|ipfs://).*$"

-- >>> encode $ Metadata $ URI "https:" (Just $ URIAuth "" "google.com" "") "/search/?q=plutus" "" ""
-- "\"https://google.com/search/?q=plutus\""

-- >>> encode $ Metadata $ URI "https:" (Just $ URIAuth "" "google.com" "") "/search/asdsadasdasdasdasdasdadadasdadadadadsasdasdsada/?q=plutus" "" ""
-- "[\"https://google.com/search/asdsadasdasdasdasdasdadadasdadadadadsa\",\"sdasdsada/?q=plutus\"]"

-- >>> encode $ Metadata $ URI "ipfs://" Nothing "abcdefghijklmnopqrstuvwxyz0123456789" "" ""
-- "\"ipfs://abcdefghijklmnopqrstuvwxyz0123456789\""

-- >>> (eitherDecode $ "\"https://asdsad.com\"") :: Either String Metadata
-- Right (Metadata {unMetadata = https://asdsad.com})

-- >>> (eitherDecode $ "[\"https://asdsad.com\",\"/asda?q=1\"]") :: Either String Metadata
-- Right (Metadata {unMetadata = https://asdsad.com/asda?q=1})

-- >>> (eitherDecode $ "\"ipfs://abcdefghijklmnopqrstuvwxyz0123456789\"") :: Either String Metadata
-- Right (Metadata {unMetadata = ipfs://abcdefghijklmnopqrstuvwxyz0123456789})

-- >>> (eitherDecode $ "\"ftp://abcdefghijklmnopqrstuvwxyz0123456789\"") :: Either String Metadata
-- Left "Error in $: Invalid URL schema ftp:"

newtype Hash = Hash { unHash :: ByteString }
             deriving (Generic,Eq,Ord)

instance Show Hash where
  show = show . unHash

instance ToJSON Hash where
  -- a 64 characters hex string
  toJSON = toBase16 . unHash
    where
    toBase16 :: ByteString -> Value
    toBase16 = String . Text.decodeUtf8

instance ToSchema Hash where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Hash") $ mempty
      & type_ ?~ SwaggerString
      & SL.pattern ?~ "^[A-Fa-f0-9]{64}$"

instance FromJSON Hash where
  parseJSON = withText "Hash" $ \t -> do
    -- if it's not a 64 characters hex string, fail
    when (Text.length t /= 64) $ fail $ "Hash must be a 64 characters hex string not: " <> show (Text.length t) <> " characters: " <> show t
    -- text to bytestring
    let bs = Text.encodeUtf8 t
    case decodeHex bs of
      Left err -> fail err
      Right _ -> pure $ Hash bs

-- >>> (decode "\"f08ccc1ee08d034d8317d1d84cab76d3cac48a8466ca9e54a291bb998c49a173\"" :: Maybe Hash)
-- Just "f08ccc1ee08d034d8317d1d84cab76d3cac48a8466ca9e54a291bb998c49a173"

-- >>> encode $ Hash "f08ccc1ee08d034d8317d1d84cab76d3cac48a8466ca9e54a291bb998c49a173"
-- "\"f08ccc1ee08d034d8317d1d84cab76d3cac48a8466ca9e54a291bb998c49a173\""
--

-- TODO: calculate hash for
data OnChainMetadata = OnChainMetadata
  { subject :: !DB.Subject
  , rootHash :: !Hash
  , metadata :: ![MetadataUrl]
  , schemaVersion :: !Int
  , onChainType :: !CertificationType
  } deriving (Generic)

instance ToJSON OnChainMetadata where
  toJSON OnChainMetadata{..} = object [
    "1304" .= object
      [ "subject" .= subject
      , "rootHash" .= rootHash
      , "metadata" .= metadata
      , "schemaVersion" .= schemaVersion
      , "type" .= onChainType
      ]
    ]

instance FromJSON OnChainMetadata where
  parseJSON = withObject "OnChainMetadata" $ \o -> do
    ocm <- o .: "1304"
    subject <- ocm .: "subject"
    rootHash <- ocm .: "rootHash"
    metadata <- ocm .: "metadata"
    schemaVersion <- ocm .: "schemaVersion"
    onChainType <- ocm .: "type"
    pure $ OnChainMetadata{..}

data Phantom1304

instance ToSchema Phantom1304 where
  declareNamedSchema _ = do
    subjectSchema <- declareSchemaRef (Proxy :: Proxy DB.Subject)
    rootHashSchema <- declareSchemaRef (Proxy :: Proxy Hash)
    metadataSchema <- declareSchemaRef (Proxy :: Proxy [MetadataUrl])
    schemaVersionSchema <- declareSchemaRef (Proxy :: Proxy Int)
    onChainTypeSchema <- declareSchemaRef (Proxy :: Proxy CertificationType)
    return $ NamedSchema (Just "1304") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("subject", subjectSchema)
          , ("rootHash", rootHashSchema)
          , ("metadata", metadataSchema)
          , ("schemaVersion", schemaVersionSchema)
          , ("type", onChainTypeSchema)
          ]
      & required .~ ["subject", "rootHash", "metadata", "schemaVersion", "type"]

instance ToSchema OnChainMetadata where
  declareNamedSchema _ = do
    _1304Schema <- declareSchemaRef (Proxy :: Proxy Phantom1304)
    return $ NamedSchema (Just "OnChainMetadata") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("1304", _1304Schema) ]
      & required .~ ["name", "social"]

type CertificationIssuerName = DB.PatternedText "CertificationIssuerName" "^.{1,64}$"

data CertificationType = CertificationType
  { certificationLevel :: !DB.CertificationLevel
  , certificateIssuer :: !CertificationIssuerName
  } deriving (Generic,Show)

instance ToJSON CertificationType where
  toJSON CertificationType{..} = object
    [ "action" .= if certificationLevel == DB.L0 then "AUDIT" :: String else "CERTIFY"
    , "certificationLevel" .= certificationLevel
    , "certificateIssuer" .= certificateIssuer
    ]

instance FromJSON CertificationType where
  parseJSON = withObject "CertificationType" $ \o -> do
    action :: String <- o .: "action"
    certificationLevel <- o .: "certificationLevel"
    _ <- case action of
      "AUDIT" | certificationLevel /= DB.L0 -> fail "AUDIT must have certificationLevel 0"
      "CERTIFY" | certificationLevel == DB.L0 -> fail "CERTIFY must have certificationLevel 1, 2 or 3"
      _ -> fail "action must be AUDIT or CERTIFY"
    certificateIssuer <- o .: "certificateIssuer"
    pure $ CertificationType{..}

data PhantomAction

instance ToSchema PhantomAction where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Action") $ mempty
      & type_ ?~ SwaggerString
      -- ACTION or CERTIFY
      & SL.pattern ?~ "^(AUDIT|CERTIFY)$"
      & description ?~ "Action must be AUDIT or CERTIFY"

instance ToSchema CertificationType where
  declareNamedSchema _ = do
    certificationLevelSchema <- declareSchemaRef (Proxy :: Proxy DB.CertificationLevel)
    certificateIssuerSchema <- declareSchemaRef (Proxy :: Proxy CertificationIssuerName)
    actionSchema <- declareSchemaRef (Proxy :: Proxy PhantomAction)
    return $ NamedSchema (Just "CertificationType") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          -- action is an enum of "AUDIT" or "CERTIFY"
          [ ("action", actionSchema)
          , ("certificationLevel", certificationLevelSchema)
          , ("certificateIssuer", certificateIssuerSchema)
          ]
      & required .~ ["action", "certificationLevel", "certificateIssuer"]

--------------------------------------------------------------------------------
-- | OFF-CHAIN METADATA
--  https://github.com/RSoulatIOHK/CIPs/blob/cip-certification-metadata/CIP-0096/README.md#off-chain-metadata-properties

type GitHubAccount = DB.PatternedText "GitHubAccount"
 "^(?=.{1,39}$)[a-zA-Z0-9]+(-[a-zA-Z0-9]+)*$"

type DiscordAccount = DB.PatternedText "DiscordAccount"
  "^.{3,32}#[0-9]{4}$"

data Social = Social
  { twitter :: !(Maybe DB.Twitter)
  , github :: !(Maybe GitHubAccount)
  , contact :: !DB.Email
  , website :: !DB.Website
  , discord :: !(Maybe DiscordAccount)
  } deriving (Show, Eq, Generic)

instance ToJSON Social where
  toJSON = genericToJSON defaultOptions

instance FromJSON Social where
  parseJSON = genericParseJSON defaultOptions

instance ToSchema Social where
  declareNamedSchema _ = do
    emailSchema <- declareSchemaRef (Proxy :: Proxy DB.Email)
    uriSchema <- declareSchemaRef (Proxy :: Proxy DB.Website)
    twitterSchema <- declareSchemaRef (Proxy :: Proxy DB.Twitter)
    githubSchema <- declareSchemaRef (Proxy :: Proxy GitHubAccount)
    discordSchema <- declareSchemaRef (Proxy :: Proxy DiscordAccount)
    return $ NamedSchema (Just "Social") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("twitter", twitterSchema)
          , ("github", githubSchema)
          , ("contact", emailSchema)
          , ("website", uriSchema)
          , ("discord", discordSchema)
          ]
      & required .~ ["contact", "website"]

data CertificateIssuer = CertificateIssuer
  { name :: !CertificationIssuerName
  , logo :: !(Maybe URL)
  , social :: !Social
  } deriving (Show, Eq, Generic)

instance ToJSON CertificateIssuer where
  toJSON = genericToJSON defaultOptions

instance FromJSON CertificateIssuer where
  parseJSON = genericParseJSON defaultOptions


instance ToSchema CertificateIssuer where
  declareNamedSchema _ = do
    uriSchema <- declareSchemaRef (Proxy :: Proxy String)
    socialSchema <- declareSchemaRef (Proxy :: Proxy Social)
    certificationIssuerName <- declareSchemaRef (Proxy :: Proxy CertificationIssuerName)
    return $ NamedSchema (Just "CertificateIssuer") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("name", certificationIssuerName )
          , ("logo", uriSchema)
          , ("social", socialSchema)
          ]
      & required .~ ["name", "social"]

newtype ReportURL = ReportURL { unReportURL :: URI } deriving (Show, Eq, Generic)

-- report url accepts only http, https, and ipfs

instance ToJSON ReportURL where
  toJSON = toJSON . show . unReportURL

instance FromJSON ReportURL where
  parseJSON = withText "ReportURL"
    (fmap ReportURL . parseMetadataURI)

instance ToSchema ReportURL where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "ReportURL") $ mempty
      & type_ ?~ SwaggerString
      & format ?~ "uri"
      & SL.pattern ?~ "^(https?|ipfs)://.*$"
      & description ?~ "Report URL"

data Report = Report
  { reportURLs :: ![ReportURL]
  , reportHash :: !Hash
  } deriving (Generic,Show)

instance ToJSON Report where
  toJSON = genericToJSON defaultOptions

instance FromJSON Report where
  -- the report has to have at least one url
  parseJSON = withObject "Report" $ \o -> do
    reportURLs <- o .: "reportURLs"
    when (Prelude.null reportURLs) $ fail "ReportURLs cannot be empty"
    reportHash <- o .: "reportHash"
    return Report{..}

instance ToSchema Report where
  declareNamedSchema _ = do
    reportURLSchema <- declareSchemaRef (Proxy :: Proxy ReportURL)
    hashSchema <- declareSchemaRef (Proxy :: Proxy Hash)
    return $ NamedSchema (Just "Report") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("reportURLs", reportURLSchema)
          , ("reportHash", hashSchema)
          ]
      & required .~ ["reportURLs", "reportHash"]

data SmartContractInfo = SmartContractInfo
  { era :: !(Maybe Text)
  , compiler :: !(Maybe Text)
  , compilerVersion :: !(Maybe Text)
  , optimizer :: !(Maybe Text)
  , optimizerVersion :: !(Maybe Text)
  , progLang :: !(Maybe Text)
  , repository :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON SmartContractInfo where
  toJSON = genericToJSON defaultOptions

instance FromJSON SmartContractInfo where
  parseJSON = genericParseJSON defaultOptions

instance ToSchema SmartContractInfo where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions


data Script = Script
  { smartContractInfo :: !(Maybe SmartContractInfo)
  , scriptHash :: !Hash
  , contractAddress :: !(Maybe ContractAddress)
  } deriving (Show, Eq, Generic)

type ContractAddress = DB.PatternedText "ContractAddress"
  "^(addr_test1|addr1)[a-zA-Z0-9]{53,}$"

instance ToJSON Script where
  toJSON = genericToJSON defaultOptions

instance FromJSON Script where
  parseJSON = genericParseJSON defaultOptions


instance ToSchema Script where
  declareNamedSchema _ = do
    smartContractInfo <- declareSchemaRef (Proxy :: Proxy SmartContractInfo)
    hashSchema <- declareSchemaRef (Proxy :: Proxy Hash)
    addressSchema <- declareSchemaRef (Proxy :: Proxy ContractAddress)
    return $ NamedSchema (Just "Script") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("smartContractInfo", smartContractInfo)
          , ("scriptHash", hashSchema)
          , ("contractAddress", addressSchema)
          ]
      & required .~ ["scriptHash"]

instance IsLabel "report" (Report -> OffChainMetadata -> OffChainMetadata) where
  fromLabel v p = p { report = v}

data OffChainMetadata = OffChainMetadata
  { subject :: !DB.Subject
  -- schemaVersion will be set to 1, but should be serialized to and from JSON
  , certificationLevel :: !DB.CertificationLevel
  , certificateIssuer :: !CertificateIssuer
  , report :: !Report
  , summary :: !Text
  , disclaimer :: !Text
  , scripts :: ![Script]
  } deriving (Generic)

instance ToJSON OffChainMetadata where
  toJSON OffChainMetadata{..} = object
      [ "subject" .= subject
      , "schemaVersion" .= (1 :: Int)
      , "certificationLevel" .= certificationLevel
      , "certificateIssuer" .= certificateIssuer
      , "report" .= report
      , "summary" .= summary
      , "disclaimer" .= disclaimer
      , "scripts" .= scripts
      ]

instance FromJSON OffChainMetadata where
  parseJSON = withObject "OffChainMetadata" $ \o -> do
    schemaVersion :: Int <- o .: "schemaVersion"
    when (schemaVersion /= 1) $ fail "schemaVersion must be 1"
    OffChainMetadata
      <$> o .: "subject"
      <*> o .: "certificationLevel"
      <*> o .: "certificateIssuer"
      <*> o .: "report"
      <*> o .: "summary"
      <*> o .: "disclaimer"
      <*> o .: "scripts"

instance ToSchema OffChainMetadata where
  declareNamedSchema _ = do
    subjectSchema <- declareSchemaRef (Proxy :: Proxy DB.Subject)
    certificationLevelSchema <- declareSchemaRef (Proxy :: Proxy DB.CertificationLevel)
    certificateIssuerSchema <- declareSchemaRef (Proxy :: Proxy CertificateIssuer)
    reportSchema <- declareSchemaRef (Proxy :: Proxy Report)
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    scriptSchema <- declareSchemaRef (Proxy :: Proxy Script)
    return $ NamedSchema (Just "OffChainMetadata") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("subject", subjectSchema)
          , ("certificationLevel", certificationLevelSchema)
          , ("certificateIssuer", certificateIssuerSchema)
          , ("report", reportSchema)
          , ("summary", textSchema)
          , ("disclaimer", textSchema)
          , ("scripts", scriptSchema)
          ]
      & required .~ ["subject", "schemaVersion", "certificationLevel", "certificateIssuer", "report", "summary", "disclaimer", "scripts"]

--------------------------------------------------------------------------------
-- | AUDITOR CERTIFICATION INPUT

data CertificationInput = CertificationInput
  { certificateIssuer :: CertificateIssuer
  , summary :: !Text
  , disclaimer :: !Text
  , scripts :: ![Script]
  } deriving (Generic,Show)

data AuditorCertificationInput = AuditorCertificationInput
  { certificationInput :: !CertificationInput
  , report :: ![ReportURL]
  , subject :: !DB.Subject
  , certificationLevel :: CertificationLevel
  } deriving (Generic,Show)

instance ToJSON AuditorCertificationInput where
  toJSON AuditorCertificationInput{..} = Object (x <> y)
    where
    x = case toJSON certificationInput of
            Object obj -> obj
            _          -> KM.empty
    y = KM.fromList [ "report" .= report, "subject" .= subject, "certificationLevel" .= certificationLevel  ]

instance FromJSON AuditorCertificationInput where
  parseJSON = withObject "AuditorCertificationInput" $ \v -> AuditorCertificationInput
      <$> parseJSON (Object v)
      <*> v .:? "report" .!= []
      <*> v .: "subject"
      <*> v .: "certificationLevel"

instance ToSchema AuditorCertificationInput where
  declareNamedSchema _ = do
    certificationInputSchema <- declareSchema (Proxy :: Proxy CertificationInput)
    reportSchema <- declareSchemaRef (Proxy :: Proxy [ReportURL])
    subjectSchema <- declareSchemaRef (Proxy :: Proxy DB.Subject)
    certificationLevelSchema <- declareSchemaRef (Proxy :: Proxy CertificationLevel)
    return $ NamedSchema (Just "AuditorCertificationInput") $ certificationInputSchema
              & properties %~ (`mappend`
                  [ ("report", reportSchema)
                  , ("subject", subjectSchema)
                  , ("certificationLevel", certificationLevelSchema)
                  ])
              & required .~ ["subject", "certificationLevel"]

instance ToJSON CertificationInput where
  toJSON = genericToJSON defaultOptions

instance FromJSON CertificationInput where
  parseJSON = genericParseJSON defaultOptions

instance ToSchema CertificationInput where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions


