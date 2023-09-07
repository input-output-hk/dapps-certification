{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}

module Plutus.Certification.GitHubClient
  ( getCommitInfo
  , Author(..)
  , Commit(..)
  , CommitDetails(..)
  , GitHubAccessToken(..)
  , RepositoryInfo(..)
  , getRepoInfo
  , AccessTokenGenerationResponse(..)
  , generateGithubAccessToken
  ) where

import Data.Aeson
import Control.Lens hiding (index, (.=))
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Data.Text
import Data.Swagger hiding (Contact,Header,Https)
import Data.Time
import IOHK.Certification.Interface
import Plutus.Certification.Internal

newtype BranchResponse = BranchResponse { commonRespCommit :: Commit }
                       deriving (Show, Generic)

data Author = Author
  { name :: !Text
  , email :: !Text
  , date :: !UTCTime
} deriving (Show, Generic)

data CommitDetails = CommitDetails
  { author :: !Author
  , message :: !Text
  , committer :: !Author
  } deriving (Show, Generic)

data Commit = Commit
  { sha :: !Text
  , commit :: !CommitDetails
  } deriving (Show, Generic)

data RepositoryInfo = RepositoryInfo
  { repoName :: !Text
  , repoOwner :: !RepositoryOwner
  , repoDefaultBranch :: !Text
  , repoDescription :: !(Maybe Text)
  , repoPrivate :: !Bool
  } deriving (Show, Generic)
    deriving (FromJSON,ToJSON) via (JSONCustomOptions 4 RepositoryInfo)

data RepositoryOwner = RepositoryOwner
  { ownerLogin :: !(Maybe Text)
  , ownerId :: !(Maybe Int)
  , ownerNodeId :: !(Maybe Text)
  , ownerAvatarUrl :: !(Maybe Text)
  , ownerGravatarId :: !(Maybe Text)
  , ownerUrl :: !(Maybe Text)
  , ownerHtmlUrl :: !(Maybe Text)
  , ownerFollowersUrl :: !(Maybe Text)
  , ownerFollowingUrl :: !(Maybe Text)
  , ownerGistsUrl :: !(Maybe Text)
  , ownerStarredUrl :: !(Maybe Text)
  , ownerSubscriptionsUrl :: !(Maybe Text)
  , ownerOrganizationsUrl :: !(Maybe Text)
  , ownerReposUrl :: !(Maybe Text)
  , ownerEventsUrl :: !(Maybe Text)
  , ownerReceivedEventsUrl :: !(Maybe Text)
  , ownerType :: !(Maybe Text)
  , ownerSiteAdmin :: !(Maybe Bool)
  } deriving (Show, Generic)
    deriving (FromJSON,ToJSON) via (JSONCustomOptions 5 RepositoryOwner)

instance ToSchema RepositoryInfo where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    ownerSchema <- declareSchemaRef (Proxy :: Proxy RepositoryOwner)
    return $ NamedSchema (Just "RepositoryInfo") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("name", textSchema)
          , ("owner", ownerSchema)
          , ("default_branch", textSchema)
          , ("description", textSchema)
          , ("private", textSchema)
          ]
      & required .~ ["name", "owner", "default_branch",  "private"]

instance ToSchema RepositoryOwner where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    return $ NamedSchema (Just "RepositoryOwner") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("login", textSchema)
          , ("id", textSchema)
          , ("node_id", textSchema)
          , ("avatar_url", textSchema)
          , ("gravatar_id", textSchema)
          , ("url", textSchema)
          , ("html_url", textSchema)
          , ("followers_url", textSchema)
          , ("following_url", textSchema)
          , ("gists_url", textSchema)
          , ("starred_url", textSchema)
          , ("subscriptions_url", textSchema)
          , ("organizations_url", textSchema)
          , ("repos_url", textSchema)
          , ("events_url", textSchema)
          , ("received_events_url", textSchema)
          , ("type", textSchema)
          , ("site_admin", textSchema)
          ]
      & required .~ []

instance FromJSON BranchResponse where
    parseJSON = withObject "BranchResponse" $ \v -> BranchResponse <$> v .: "commit"

instance FromJSON Commit
instance FromJSON CommitDetails
instance FromJSON Author

type RepoAPI = "repos"
         :> Header "User-Agent" Text
         :> Header "Authorization" GitHubAccessTokenWrapper
         :> Capture "owner" Text
         :> Capture "repo" Text
         :> (  "branches" :> Capture "branch" Text :> Get '[JSON] BranchResponse
          :<|> "commits" :> Capture "commit" Text :> Get '[JSON] Commit
          :<|> Get '[JSON] RepositoryInfo
            )


api :: Proxy RepoAPI
api = Proxy

type Repo = Text
type Owner = Text

newtype GitHubAccessTokenWrapper = GitHubAccessTokenWrapper { unwrapToken :: GitHubAccessToken }

mkRepoClient :: Maybe GitHubAccessToken
             -> Repo
             -> Owner
             -> (Text -> ClientM BranchResponse)
              :<|> (Text -> ClientM Commit)
              :<|> ClientM RepositoryInfo
mkRepoClient token = client api (Just "") (GitHubAccessTokenWrapper <$> token)

instance ToHttpApiData GitHubAccessTokenWrapper where
  toUrlPiece = ("Bearer " <>) . pack . show . unwrapToken

githubApiBaseUrl :: BaseUrl
githubApiBaseUrl = BaseUrl Https "api.github.com" 443 ""

createSettings :: BaseUrl -> IO ClientEnv
createSettings url' = flip mkClientEnv url' <$> newManager tlsManagerSettings

-- | Binds the client to a specific owner , repo and github access token
-- also applies same settings and https github domain
-- returns a tuple of 3 functions: getBranch, getCommit, getRepo
bindRepoClient :: Maybe GitHubAccessToken
           -> Repo
           -> Owner
           -> ( Text -> IO (Either ClientError BranchResponse)
              , Text -> IO (Either ClientError Commit)
              , IO (Either ClientError RepositoryInfo)
              )
bindRepoClient githubAccessToken owner repo =
  ( \path' -> settings >>= runClientM (getBranch path' )
  , \path' -> settings >>= runClientM (getCommit path' )
  , settings >>= runClientM getRepo
  )
  where
  settings = createSettings githubApiBaseUrl
  getBranch :<|> getCommit :<|> getRepo = mkRepoClient githubAccessToken owner repo

-- | Tries to get the commit info from a branch,
-- if it fails it tries to get it from the commit
getCommitInfo :: Maybe GitHubAccessToken
              -> Repo
              -> Owner
              -> Text
              -> IO (Either ClientError Commit)
getCommitInfo githubAccessToken owner repo path' = do
  let (getBranch, getCommit,_) = bindRepoClient githubAccessToken owner repo
  -- first try branch
  respE <- getBranch path'
  case respE of
    Left _ -> getCommit path'
    Right (BranchResponse commit') -> pure (Right commit')

-- | Tries to get the github repository info
getRepoInfo :: Maybe GitHubAccessToken
            -> Repo
            -> Owner
            -> IO (Either ClientError RepositoryInfo)
getRepoInfo githubAccessToken owner repo = do
  let (_, _, getRepo') = bindRepoClient githubAccessToken owner repo
  getRepo'

--------------------------------------------------------------------------------
-- | GITHUB AUTH TOKEN

-- an example to get an access token
type AccessTokenAPI
  = "login"
  :> "oauth"
  :> "access_token"
  :> Header "User-Agent" Text
  :> QueryParam "client_id" Text
  :> QueryParam "client_secret" Text
  :> QueryParam "code" Text
  :> Post '[JSON] AccessTokenGenerationResponse

accessTokenAPI :: Proxy AccessTokenAPI
accessTokenAPI = Proxy

data AccessTokenGenerationResponse = AccessTokenGenerationResponse
  { accessToken :: !GitHubAccessToken
  , tokenType :: !(Maybe Text)
  , scope :: !(Maybe Text)
  } deriving (Generic)

instance FromJSON AccessTokenGenerationResponse where
    parseJSON = withObject "AccessTokenGenerationResponse" $ \v -> do
      accessToken' <- v .: "access_token"
      case ghAccessTokenFromText accessToken' of
        Left err -> fail err
        Right ghAccessToken -> AccessTokenGenerationResponse ghAccessToken
          <$> v .: "token_type"
          <*> v .: "scope"

instance ToSchema AccessTokenGenerationResponse where
   declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)
    textSchemaM <- declareSchemaRef (Proxy :: Proxy (Maybe Text))
    return $ NamedSchema (Just "AccessTokenGenerationResponse") $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("access_token", textSchema)
          , ("token_type", textSchemaM)
          , ("scope", textSchemaM)
          ]
      & required .~ [ "access_token" ]

instance ToJSON AccessTokenGenerationResponse where
  toJSON (AccessTokenGenerationResponse accessToken' tokenType' scope') =
    object [ "access_token" .= show accessToken'
           , "token_type" .= tokenType'
           , "scope" .= scope'
           ]
type ClientId = Text
type ClientSecret = Text
type Code = Text

-- | Generates a github access token based on the client id,
-- client secret and code
generateGithubAccessToken :: ClientId       -- ^ client id
                          -> ClientSecret   -- ^ client secret
                          -> Code           -- ^ code
                          -> IO (Either ClientError AccessTokenGenerationResponse)
generateGithubAccessToken clientId clientSecret code = do
  settings <- createSettings baseUrl'
  let client' = client accessTokenAPI (Just "") (Just clientId) (Just clientSecret) (Just code)
  runClientM client' settings
  where
  baseUrl' = BaseUrl Https "github.com" 443 ""
