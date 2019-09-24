{-|
Module      :  GitHub.REST.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines 'GitHubT' and 'MonadGitHubREST', a monad transformer and type class that gives a monad @m@
the capability to query the GitHub REST API.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GitHub.REST.Monad
  ( MonadGitHubREST(..)
  , GitHubT
  , GitHubState(..)
  , runGitHubT
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client
    ( Manager
    , Request(..)
    , RequestBody(..)
    , Response(..)
    , httpLbs
    , newManager
    , parseRequest_
    , throwErrorStatusCodes
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent)

import GitHub.REST.Auth (Token, fromToken)
import GitHub.REST.Endpoint (GHEndpoint(..), endpointPath, renderMethod)
import GitHub.REST.KeyValue (kvToValue)
import GitHub.REST.Monad.Class

data GitHubState = GitHubState
  { token      :: Token
  , userAgent  :: ByteString
  , apiVersion :: ByteString
    -- ^ The media type will be sent as: application/vnd.github.VERSION+json. For the standard
    -- API endpoints, "v3" should be sufficient here. See https://developer.github.com/v3/media/
  }

-- | A simple monad that can run REST calls.
newtype GitHubT m a = GitHubT
  { unGitHubT :: ReaderT (Manager, GitHubState) m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadTrans
    )

instance MonadUnliftIO m => MonadUnliftIO (GitHubT m) where
  askUnliftIO = GitHubT $
    withUnliftIO $ \u ->
      return $ UnliftIO (unliftIO u . unGitHubT)

instance MonadIO m => MonadGitHubREST (GitHubT m) where
  queryGitHubPage' ghEndpoint = do
    (manager, GitHubState{..}) <- GitHubT ask

    let request = (parseRequest_ $ Text.unpack $ ghUrl <> endpointPath ghEndpoint)
          { method = renderMethod ghEndpoint
          , requestHeaders =
              [ (hAccept, "application/vnd.github." <> apiVersion <> "+json")
              , (hUserAgent, userAgent)
              , (hAuthorization, fromToken token)
              ]
          , requestBody = RequestBodyLBS $ encode $ kvToValue $ ghData ghEndpoint
          , checkResponse = throwErrorStatusCodes
          }

    response <- liftIO $ httpLbs request manager

    let body = responseBody response
        -- empty body always errors when decoding, even if the end user doesn't care about the
        -- result, like creating a branch, when the endpoint doesn't return anything.
        --
        -- In this case, pretend like the server sent back an encoded version of the unit type,
        -- so that `queryGitHub endpoint` would be typed to `m ()`.
        nonEmptyBody = if ByteStringL.null body then encode () else body
        pageLinks = parsePageLinks $ responseHeaders response

    return $ case eitherDecode nonEmptyBody of
      Right payload -> Right (payload, pageLinks)
      Left e -> Left (Text.pack e, Text.decodeUtf8 $ ByteStringL.toStrict body)
    where
      ghUrl = "https://api.github.com"

      parsePageLinks headers =
        let split delim = map Text.strip . Text.splitOn delim
            dropAround begin end s =
              fromMaybe
                (error $ "Expected value to wrap within " ++ Text.unpack begin ++ "..." ++ Text.unpack end ++ ": " ++ Text.unpack s)
                (Text.stripSuffix end =<< Text.stripPrefix begin s)

            parsePageLink link =
              case split ";" link of
                [url, rel] ->
                  let url' = fromMaybe
                        (error $ "Unknown page link: " ++ show link)
                        (Text.stripPrefix ghUrl $ dropAround "<" ">" url)
                  in case split "=" rel of
                    ["rel", rel'] -> (dropAround "\"" "\"" rel', url')
                    _ -> error $ "Unknown page link: " ++ show link
                _ -> error $ "Unknown page link: " ++ show link

            resolve pageLinks "" = pageLinks
            resolve pageLinks link =
              let (rel, url) = parsePageLink link
              in case rel of
                "first" -> pageLinks { pageFirst = Just url }
                "prev" -> pageLinks { pagePrev = Just url }
                "next" -> pageLinks { pageNext = Just url }
                "last" -> pageLinks { pageLast = Just url }
                _ -> error $ "Unknown rel in page link: " ++ show link

            linkHeader = Text.decodeUtf8 . fromMaybe "" . lookup "Link" $ headers

        in foldl resolve mempty . split "," $ linkHeader

-- | Run the given 'GitHubT' action with the given token and user agent.
--
-- The token will be sent with each API request -- see 'Token'. The user agent is also required for
-- each API request -- see https://developer.github.com/v3/#user-agent-required.
runGitHubT :: MonadIO m => GitHubState -> GitHubT m a -> m a
runGitHubT state action = do
  manager <- liftIO $ newManager tlsManagerSettings
  (`runReaderT` (manager, state)) . unGitHubT $ action
