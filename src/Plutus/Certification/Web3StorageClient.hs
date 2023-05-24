{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plutus.Certification.Web3StorageClient where

import Data.Aeson
import Network.HTTP.Client      hiding (Proxy)
import Network.HTTP.Client.TLS
import Control.Monad.IO.Class
import Network.HTTP.Types.Header
import Data.ByteString.Char8 as BS hiding (hPutStrLn)
import Servant.Client.Core hiding (responseBody,Response,DecodeFailure)
import Data.Text as Text
import Servant.API hiding (addHeader)
import Network.HTTP.Types.Status
import Network.HTTP.Client.MultipartFormData
import System.IO.Temp
import IOHK.Certification.Persistence
import qualified Data.ByteString.Lazy.Char8 as BSL

import GHC.IO.Handle
type instance AuthClientData (AuthProtect "api-key") = ByteString

addAuth :: ByteString -> AuthenticatedRequest (AuthProtect "api-key")
addAuth = flip mkAuthenticatedRequest (\v -> addHeader hAuthorization ("Bearer " <> BS.unpack v))

data UploadResponse = UploadResponse
  { uploadCid :: IpfsCid
  , uploadCarCid :: Text
  } deriving Show

instance FromJSON UploadResponse where
  parseJSON = withObject "UploadResponse" $ \o -> UploadResponse
    <$> o .: "cid"
    <*> o .: "carCid"

--TODO: this should be replaced with a configured key.
apiKey :: ByteString
apiKey = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJkaWQ6ZXRocjoweDE5ZWJFN0VFNUM3MUE0NUEyOEYzYjlCMjNFNzIwM0Y4MThmNGUyNmUiLCJpc3MiOiJ3ZWIzLXN0b3JhZ2UiLCJpYXQiOjE2NzA0MzU3MTg4MzAsIm5hbWUiOiJJT0cifQ.J3xzOYClx7qcOrTZErsfuMs9U9RchINAUJNzyEInsGQ"

data UploadReportError
  = DecodeFailure (Response BSL.ByteString) String
  | HttpError (Response BSL.ByteString)
  deriving Show

uploadReportToIpfs :: MonadIO m => ByteString -> ByteString -> m (Either UploadReportError UploadResponse)
uploadReportToIpfs key content = liftIO $ withSystemTempFile "certification-report.json" $ \fn handle -> do
  manager' <- newManager tlsManagerSettings
  BS.hPutStr handle content >> hClose handle
  let form = [ partFileSource "file" fn ]
  req <- applyBearerAuth key <$> parseRequest "https://api.web3.storage/upload"
  resp <- flip httpLbs manager' =<< formDataBody form req
  pure $ case  responseStatus resp of
    Status 200 _ -> either (Left . DecodeFailure resp ) Right (eitherDecode' (responseBody resp))
    _ -> Left $ HttpError resp

