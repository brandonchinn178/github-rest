{-|
Module      :  GitHub.REST
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for querying the GitHub REST API.

Example (requires @OverloadedStrings@):

> import Data.Monoid ((<>))
> import Data.Text (Text)
> import GitHub.REST
> import Network.HTTP.Types (StdMethod(..))
>
> main = do
>   let repoOwner = "alice"
>       repoName = "my-project"
>       state = GitHubState
>         { token = Nothing
>         , userAgent = repoOwner <> "/" <> repoName
>         , apiVersion = "v3"
>         }
>
>   runGitHubT state $ do
>     ref <- queryGitHub GHEndpoint
>       { method = GET
>       , endpoint = "/repos/:owner/:repo/git/refs/:ref"
>       , endpointVals =
>         [ "owner" := repoOwner
>         , "repo" := repoName
>         , "ref" := ("heads/master" :: Text)
>         ]
>       , ghData = []
>       }
>     let sha = ref .: "object" .: "sha" :: Text
>
>     queryGitHub GHEndpoint
>       { method = POST
>       , endpoint = "/repos/:owner/:repo/git/refs"
>       , endpointVals =
>         [ "owner" := repoOwner
>         , "repo" := repoName
>         ]
>       , ghData =
>         [ "ref" := ("refs/heads/foo" :: Text)
>         , "sha" := sha
>         ]
>       }
-}

module GitHub.REST
  (
  -- * Monad transformer and type-class for querying the GitHub REST API
    MonadGitHubREST(..)
  , GitHubT
  , GitHubState(..)
  , runGitHubT
  -- * GitHub authentication
  , Token(..)
  -- * GitHub Endpoints
  , GHEndpoint(..)
  , GitHubData
  , EndpointVals
  -- * KeyValue pairs
  , KeyValue(..)
  -- * Helpers
  , githubTry
  , githubTry'
  , (.:)
  -- * Re-exports
  , StdMethod(..)
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (FromJSON, Value(..), decode, withObject)
import Data.Aeson.Types (parseEither, parseField)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Text (Text)
import Network.HTTP.Client
    (HttpException(..), HttpExceptionContent(..), Response(..))
import Network.HTTP.Types (Status, StdMethod(..), status422)
import UnliftIO.Exception (handleJust)

import GitHub.REST.Auth
import GitHub.REST.Endpoint
import GitHub.REST.KeyValue
import GitHub.REST.Monad

{- HTTP exception handling -}

-- | Handle 422 exceptions thrown by the GitHub REST API.
--
-- Most client errors are 422, since we should always be sending valid JSON. If an endpoint
-- throws different error codes, use githubTry'.
--
-- https://developer.github.com/v3/#client-errors
githubTry :: MonadUnliftIO m => m a -> m (Either Value a)
githubTry = githubTry' status422

-- | Handle the given exception thrown by the GitHub REST API.
githubTry' :: MonadUnliftIO m => Status -> m a -> m (Either Value a)
githubTry' status = handleJust statusException (return . Left) . fmap Right
  where
    statusException (HttpExceptionRequest _ (StatusCodeException r body))
      | responseStatus r == status = decode $ ByteStringL.fromStrict body
    statusException _ = Nothing

{- Aeson helpers -}

-- | Get the given key from the Value, erroring if it doesn't exist.
(.:) :: FromJSON a => Value -> Text -> a
(.:) v key = either error id $ parseEither parseObject v
  where
    parseObject = withObject "parseObject" (`parseField` key)
