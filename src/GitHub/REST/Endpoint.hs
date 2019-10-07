{-|
Module      :  GitHub.REST.Endpoint
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Define the 'GHEndpoint' helper type for defining a call to a GitHub API endpoint.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GitHub.REST.Endpoint
  ( GHEndpoint(..)
  , EndpointVals
  , GitHubData
  , endpointPath
  , renderMethod
  ) where

import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Types (Method, StdMethod, renderStdMethod)

import GitHub.REST.KeyValue (KeyValue, kvToText)

type EndpointVals = [KeyValue]
type GitHubData = [KeyValue]

-- | A call to a GitHub API endpoint.
data GHEndpoint = GHEndpoint
  { method       :: StdMethod
  , endpoint     :: Text
    -- ^ The GitHub API endpoint, with colon-prefixed components that will be replaced; e.g.
    -- @"\/users\/:username\/repos"@
  , endpointVals :: EndpointVals
    -- ^ Key-value pairs to replace colon-prefixed components in 'endpoint'; e.g.
    -- @[ "username" := ("alice" :: Text) ]@
  , ghData       :: GitHubData
    -- ^ Key-value pairs to send in the request body; e.g.
    -- @[ "sort" := ("created" :: Text), "direction" := ("asc" :: Text) ]@
  }

-- | Return the endpoint path, populated by the values in 'endpointVals'.
endpointPath :: GHEndpoint -> Text
endpointPath GHEndpoint{..} = Text.intercalate "/" . map populate . Text.splitOn "/" $ endpoint
  where
    values = map kvToText endpointVals
    populate t = case Text.uncons t of
      Just (':', key) -> fromMaybe
        (fail' $ "Could not find value for key '" <> key <> "'")
        $ lookup key values
      _ -> t
    fail' msg = error . Text.unpack $ msg <> ": " <> endpoint

-- | Render the method of the endpoint.
renderMethod :: GHEndpoint -> Method
renderMethod = renderStdMethod . method
