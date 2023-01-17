{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Certification.GithubClient (
  getCommitInfo,
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

data BranchResponse = BranchResponse {
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

instance FromJSON BranchResponse where
    parseJSON = withObject "BranchResponse" $ \v -> BranchResponse <$> v .: "commit"

instance FromJSON Commit
instance FromJSON CommitDetails
instance FromJSON Author

type API = "repos"
         :> Header "User-Agent" Text
         :> Capture "owner" Text
         :> Capture "repo" Text
         :> (  "branches" :> Capture "branch" Text :> Get '[JSON] BranchResponse
          :<|> "commits" :> Capture "commit" Text :> Get '[JSON] Commit
            )

api :: Proxy API
api = Proxy

mkClient :: Text -> Text -> (Text -> ClientM BranchResponse) :<|> (Text -> ClientM Commit)
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
  respE <- runClientM (getBranch path' ) settings
  case respE of
    Left _ -> runClientM (getCommit path' ) settings
    Right (BranchResponse commit') -> pure (Right commit')

