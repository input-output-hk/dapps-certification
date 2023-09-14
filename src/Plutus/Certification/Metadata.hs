{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Plutus.Certification.Metadata
( module X
, createDraftMetadata
, createMetadataAndPushToIpfs
, FullMetadata(..)
, parseURIUnsafe
, CertificationInput(..)
, AuditorCertificationInput(..)
, URL(..)
, DB.Subject
) where

import Plutus.Certification.Metadata.Types as X
import Crypto.Hash (Digest, Blake2b_256(..), hashWith, )
import Network.HTTP.Conduit (simpleHttp)
import Control.Lens         hiding (index, (.=))
import Network.HTTP.Types
import Data.Proxy
import Network.HTTP.Client hiding (Proxy,parseUrl)
import Data.Swagger hiding (summary)

import Control.Monad.IO.Class (MonadIO)
import Data.List (nub)
import Control.Exception (throw)
import Data.Aeson
import Data.Text as Text hiding (head,length)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Plutus.Certification.Web3StorageClient as IPFS
import IOHK.Certification.Persistence (IpfsCid(..))
import Network.URI (parseURI, URI (uriScheme))
import Data.Maybe (fromMaybe)
import qualified IOHK.Certification.Persistence as DB


-- TODO: we need to set a maximum size for the report
hashURL :: MonadIO m => String -> m (Digest Blake2b_256)
hashURL url' = hashContent <$> simpleHttp (toHttpUrl url')

hashContent :: LBS.ByteString -> Digest Blake2b_256
hashContent = hashWith Blake2b_256 . LBS.toStrict

throwError :: String -> c
throwError = throw . userError

-- TODO: post to ipfs if it's the
toReport :: MonadIO m => [ReportURL] -> m Report
toReport [] = throwError "No report URLs provided"
toReport urls = do
    xs <- mapM (hashURL . show . unReportURL) urls
    if allSame xs
        then pure $ Report urls ( digestToHash $ head xs)
        else throwError "Report URLs do not match"

digestToHash :: Digest a -> Hash
digestToHash = Hash . BS.pack . show

allSame :: Eq a => [a] -> Bool
allSame xs = length (nub xs) <= 1

parseURIUnsafe :: String -> URI
parseURIUnsafe = fromMaybe (throwError "Invalid URI") . parseURI

{-
>>> xsToReps str = fmap (ReportURL . parseURIUnsafe) str

>>> toReport $ xsToReps ["https://google.com", "https://google.com"]
user error (Report URLs do not match)
>>> toReport $ xsToReps ["ipfs://bafkreihic53arwawc73rivbnxl3ax2cc26xvd23x67obm5vq33uosdwbcy", "https://bafkreihic53arwawc73rivbnxl3ax2cc26xvd23x67obm5vq33uosdwbcy.ipfs.w3s.link"]
Report {reportURLs = [ReportURL {unReportURL = ipfs://bafkreihic53arwawc73rivbnxl3ax2cc26xvd23x67obm5vq33uosdwbcy},ReportURL {unReportURL = https://bafkreihic53arwawc73rivbnxl3ax2cc26xvd23x67obm5vq33uosdwbcy.ipfs.w3s.link}], reportHash = "64663839663138363636393838333631346534613932333265346235643236376337396531346435316266643836383263616661623536396663376433313463"}
-}

--certLevel :: DB.CertificationLevel
--certLevel = DB.L2

createOffchainMetadata :: (MonadIO m)
                       => AuditorCertificationInput
                       -> AllowNoReport
                       -> m OffChainMetadata
createOffchainMetadata AuditorCertificationInput{..} allowNoReport = do
  let CertificationInput{..} = certificationInput
  report' <- if allowNoReport && Prelude.null report
                then pure (Report [] (Hash ""))
                else toReport report
  pure OffChainMetadata { report = report', ..}

createOnchainMetadata :: AuditorCertificationInput
                      -> Maybe (OffChainMetadata,[MetadataUrl])
                      -> OnChainMetadata
createOnchainMetadata AuditorCertificationInput{..} offchainM =
  let CertificationInput{..} = certificationInput
      (rootHash,metadata') = case offchainM of
        Nothing -> (Hash "",[])
        Just (offchain,metadata) ->
          -- hash represents the encoded json of the offchain metadata
          let hash = hashContent $ encode offchain
          in (digestToHash hash,metadata)
      certificateIssuerName = certificateIssuer.name
  in OnChainMetadata
      { metadata=metadata'
      , schemaVersion = 1
      , onChainType = CertificationType
        { certificationLevel = certificationLevel
        , certificateIssuer = certificateIssuerName
        }
      ,..}

newtype FullMetadata = FullMetadata (OnChainMetadata,OffChainMetadata)

instance ToJSON FullMetadata where
  toJSON (FullMetadata (onchain,offchain)) =
    object
      [ "onchain" .= onchain
      , "offchain" .= offchain
      ]

instance FromJSON FullMetadata where
  parseJSON = withObject "FullMetadata" \o -> do
    onchain <- o .: "onchain"
    offchain <- o .: "offchain"
    pure $ FullMetadata (onchain,offchain)

instance ToSchema FullMetadata where
  declareNamedSchema _ = do
    onChainMetadataSchema <- declareSchemaRef (Proxy :: Proxy OnChainMetadata)
    offChainMetadataSchema <- declareSchemaRef (Proxy :: Proxy OffChainMetadata)
    return $ NamedSchema (Just "FullMetadata") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("onchain", onChainMetadataSchema)
          , ("offchain", offChainMetadataSchema)
          ]
      & required .~ ["onchain","offchain"]

type AllowNoReport = Bool
createDraftMetadata :: MonadIO m
                    => AuditorCertificationInput
                    -> AllowNoReport
                    -> m FullMetadata
createDraftMetadata input allowNoReport = do
  offchain <- createOffchainMetadata input allowNoReport
  let onchain = createOnchainMetadata input Nothing
  pure $ FullMetadata (onchain,offchain)

createMetadataAndPushToIpfs :: MonadIO m
                            => AuditorCertificationInput
                            -> m (FullMetadata,IpfsCid)
createMetadataAndPushToIpfs input = do
  offchain <- createOffchainMetadata input False
  finalOffchain <- addIpfsToMetadataIfNecessary offchain
  ipfsCid <- uploadToIpfs finalOffchain
  let onchain = createOnchainMetadata input
        (Just (finalOffchain,[toMetadataUrl ipfsCid]))
  pure (FullMetadata (onchain,finalOffchain),IpfsCid ipfsCid)
  where
  addIpfsToMetadataIfNecessary (offchain :: OffChainMetadata) = do
    let Report{reportURLs} = offchain.report
    -- if there isn't any ipfs url in the report, add the ipfs url to the metadata
    if Prelude.any (isIpfsUrl . unReportURL) reportURLs
       then pure offchain
       else do
        -- get the ipfs cid of the first url
        let url' = show . unReportURL . head $ offchain.report.reportURLs
        bs <- simpleHttp (toHttpUrl url')
        ipfsCid <- uploadToIpfsBS (LBS.toStrict bs)
        let reportUrl = ReportURL $ parseURIUnsafe ("ipfs://" <> Text.unpack ipfsCid)
        pure $ offchain { report = (offchain.report) { reportURLs = reportURLs <> [reportUrl] }}
  isIpfsUrl uri = case uriScheme uri of
    "ipfs:" -> True
    _ -> False

toMetadataUrl :: Text -> MetadataUrl
toMetadataUrl ipfsCid = MetadataUrl $ parseURIUnsafe $ "ipfs://" <> Text.unpack ipfsCid

uploadToIpfs :: (MonadIO m, ToJSON a) => a -> m Text
uploadToIpfs obj = do
  uploadToIpfsBS (LBS.toStrict $ encode obj)

uploadToIpfsBS :: (MonadIO m) => BS.ByteString -> m Text
uploadToIpfsBS bs = do
  resp <- IPFS.uploadReportToIpfs IPFS.apiKey bs
  case resp of
    Left (IPFS.DecodeFailure _ err) -> throwError err
    Left (IPFS.HttpError resp') ->
      let (Status code msg) = responseStatus resp'
          err = "IPFS gateway error code: " <> show code  <> ", msg: " <> BS.unpack msg
      in throwError err
    Right (IPFS.UploadResponse (IpfsCid ipfsCid) _)-> pure ipfsCid


-- >>> toHttpUrl "ipfs://bafkreihic53arwawc73rivbnxl3ax2cc26xvd23x67obm5vq33uosdwbcy"
-- "https://bafkreihic53arwawc73rivbnxl3ax2cc26xvd23x67obm5vq33uosdwbcy.ipfs.w3s.link"

-- >>> hashURL "https://google.com"
-- a17cc1eb1a10e563e2778a154e8d16af0935965acc94e462286cc7396b2bad46

-- >>> hashURL "https://bafkreihic53arwawc73rivbnxl3ax2cc26xvd23x67obm5vq33uosdwbcy.ipfs.w3s.link"
-- df89f186669883614e4a9232e4b5d267c79e14d51bfd8682cafab569fc7d314c

toHttpUrl :: String -> String
toHttpUrl ('i':'p':'f':'s':':':'/':'/':rest) = "https://" <> rest <> ".ipfs.w3s.link"
toHttpUrl url' = url'

