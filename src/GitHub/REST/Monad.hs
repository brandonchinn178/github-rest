{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      :  GitHub.REST.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines 'GitHubT' and 'MonadGitHubREST', a monad transformer and type class that gives a monad @m@
the capability to query the GitHub REST API.
-}
module GitHub.REST.Monad (
  -- * MonadGitHubREST API
  MonadGitHubREST (..),
  queryGitHubPageIO,

  -- * GitHubManager
  GitHubManager,
  initGitHubManager,

  -- * GitHubSettings
  GitHubSettings (..),

  -- * GitHubT
  GitHubT,
  runGitHubT,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Data.Aeson (FromJSON, eitherDecode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (
  Manager,
  Request (..),
  RequestBody (..),
  Response (..),
  httpLbs,
  newManager,
  parseRequest_,
  throwErrorStatusCodes,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent)
import UnliftIO.Exception (Exception, throwIO)

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

import GitHub.REST.Auth (Token, fromToken)
import GitHub.REST.Endpoint (GHEndpoint (..), endpointPath, renderMethod)
import GitHub.REST.KeyValue (kvToValue)
import GitHub.REST.Monad.Class
import GitHub.REST.PageLinks (PageLinks, parsePageLinks)

{- GitHubSettings -}

data GitHubSettings = GitHubSettings
  { -- | The token to use to authenticate with the API.
    token :: Maybe Token
  , -- | The user agent to use when interacting with the API: https://developer.github.com/v3/#user-agent-required
    userAgent :: ByteString
  , -- | The media type will be sent as: application/vnd.github.VERSION+json. For the standard
    -- API endpoints, "v3" should be sufficient here. See https://developer.github.com/v3/media/
    apiVersion :: ByteString
  }

{- GitHubManager -}

data GitHubManager = GitHubManager
  { ghSettings :: GitHubSettings
  , ghManager :: Manager
  }

-- | Initialize a 'GitHubManager'.
initGitHubManager :: GitHubSettings -> IO GitHubManager
initGitHubManager ghSettings = do
  ghManager <- newManager tlsManagerSettings
  return GitHubManager{..}

{- | Same as 'queryGitHubPage', except explicitly taking in 'GitHubManager' and running
 in IO.

 Useful for implementing 'MonadGitHubREST' outside of 'GitHubT'.
-}
queryGitHubPageIO :: FromJSON a => GitHubManager -> GHEndpoint -> IO (a, PageLinks)
queryGitHubPageIO GitHubManager{..} ghEndpoint = do
  let GitHubSettings{..} = ghSettings

  let request =
        (parseRequest_ $ Text.unpack $ ghUrl <> endpointPath ghEndpoint)
          { method = renderMethod ghEndpoint
          , requestHeaders =
              [ (hAccept, "application/vnd.github." <> apiVersion <> "+json")
              , (hUserAgent, userAgent)
              ]
                ++ maybe [] ((: []) . (hAuthorization,) . fromToken) token
          , requestBody = RequestBodyLBS $ encode $ kvToValue $ ghData ghEndpoint
          , checkResponse = throwErrorStatusCodes
          }

  response <- httpLbs request ghManager

  let body = responseBody response
      -- empty body always errors when decoding, even if the end user doesn't care about the
      -- result, like creating a branch, when the endpoint doesn't return anything.
      --
      -- In this case, pretend like the server sent back an encoded version of the unit type,
      -- so that `queryGitHub endpoint` would be typed to `m ()`.
      nonEmptyBody = if ByteStringL.null body then encode () else body
      pageLinks = maybe mempty parsePageLinks . lookupHeader "Link" $ response

  case eitherDecode nonEmptyBody of
    Right payload -> return (payload, pageLinks)
    Left e ->
      throwIO $
        DecodeError
          { decodeErrorMessage = Text.pack e
          , decodeErrorResponse = Text.decodeUtf8 $ ByteStringL.toStrict body
          }
  where
    ghUrl = "https://api.github.com"
    lookupHeader headerName = fmap Text.decodeUtf8 . lookup headerName . responseHeaders

data DecodeError = DecodeError
  { decodeErrorMessage :: Text
  , decodeErrorResponse :: Text
  }
  deriving (Show)

instance Exception DecodeError

{- GitHubT -}

-- | A simple monad that can run REST calls.
newtype GitHubT m a = GitHubT
  { unGitHubT :: ReaderT GitHubManager m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadIO
    , MonadTrans
    )

instance MonadUnliftIO m => MonadUnliftIO (GitHubT m) where
  withRunInIO inner = GitHubT $
    withRunInIO $ \run ->
      inner (run . unGitHubT)

instance MonadIO m => MonadGitHubREST (GitHubT m) where
  queryGitHubPage ghEndpoint = do
    manager <- GitHubT ask
    liftIO $ queryGitHubPageIO manager ghEndpoint

{- | Run the given 'GitHubT' action with the given token and user agent.

 The token will be sent with each API request -- see 'Token'. The user agent is also required for
 each API request -- see https://developer.github.com/v3/#user-agent-required.
-}
runGitHubT :: MonadIO m => GitHubSettings -> GitHubT m a -> m a
runGitHubT settings action = do
  manager <- liftIO $ initGitHubManager settings
  (`runReaderT` manager) . unGitHubT $ action
