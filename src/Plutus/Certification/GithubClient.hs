{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Certification.GithubClient (
  getCommitInfo,
  CommonResponse(..),
  Author(..),
  Commit(..),
  CommitDetails(..)
  ) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client      hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Data.Text
import Data.Time

data CommonResponse = CommonResponse {
    commonRespCommit :: Commit
} deriving (Show, Generic)

data Author = Author
  { name :: Text
  , email :: Text
  , date :: UTCTime
} deriving (Show, Generic)

data CommitDetails = CommitDetails
  { author :: Author
  , message :: Text
  , committer :: Author
  } deriving (Show, Generic)

data Commit = Commit
  { sha :: Text
  , commit :: CommitDetails
  } deriving (Show, Generic)

instance FromJSON CommonResponse where
    parseJSON = withObject "CommonResponse" $ \v -> CommonResponse <$> v .: "commit"

instance FromJSON Commit
instance FromJSON CommitDetails
instance FromJSON Author

type API = "repos"
         :> Header "User-Agent" Text
         :> Capture "owner" Text
         :> Capture "repo" Text
         :> (  "branches" :> Capture "branch" Text :> Get '[JSON] CommonResponse
          :<|> "commits" :> Capture "commit" Text :> Get '[JSON] CommonResponse
            )

api :: Proxy API
api = Proxy

mkClient :: Text -> Text -> (Text -> ClientM CommonResponse) :<|> (Text -> ClientM CommonResponse)
mkClient = (client api) (Just "")

getCommitInfo :: Text
              -> Text
              -> Text
              -> IO (Either ClientError Commit)
getCommitInfo owner repo path' = do
  manager' <- newManager tlsManagerSettings
  let settings  = (mkClientEnv manager' (BaseUrl Https "api.github.com" 443 ""))
  let getBranch :<|> getCommit = mkClient owner repo

  -- first try branch
  fmap commonRespCommit <$> do
        respE <- runClientM (getBranch path' ) settings
        case respE of
          Left _ -> runClientM (getCommit path' ) settings
          _ -> pure respE

