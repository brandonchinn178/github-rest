{-|
Module      :  GitHub.REST.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines 'MonadGitHubREST' that gives a monad @m@ the capability to query the GitHub REST API.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module GitHub.REST.Monad.Class
  ( MonadGitHubREST(..)
  ) where

import Control.Monad (void, (<=<))
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Aeson (FromJSON, Value)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as Text

import GitHub.REST.Endpoint
import GitHub.REST.PageLinks (PageLinks(..))

-- | A type class for monads that can query the GitHub REST API.
--
-- Example:
--
-- > -- create the "foo" branch
-- > queryGitHub GHEndpoint
-- >   { method = POST
-- >   , endpoint = "/repos/:owner/:repo/git/refs"
-- >   , endpointVals =
-- >     [ "owner" := "alice"
-- >     , "repo" := "my-project"
-- >     ]
-- >   , ghData =
-- >     [ "ref" := "refs/heads/foo"
-- >     , "sha" := "1234567890abcdef"
-- >     ]
-- >   }
--
-- It's recommended that you create functions for the API endpoints you're using:
--
-- > deleteBranch branch = queryGitHub GHEndpoint
-- >   { method = DELETE
-- >   , endpoint = "/repos/:owner/:repo/git/refs/:ref"
-- >   , endpointVals =
-- >     [ "owner" := "alice"
-- >     , "repo" := "my-project"
-- >     , "ref" := "heads/" <> branch
-- >     ]
-- >   , ghData = []
-- >   }
class MonadFail m => MonadGitHubREST m where
  {-# MINIMAL queryGitHubPage' #-}

  -- | Query GitHub, returning @Right (payload, links)@ if successful, where @payload@ is the
  -- response that GitHub sent back and @links@ containing any pagination links GitHub may have
  -- sent back. If the response could not be decoded as JSON, returns
  -- @Left (error message, response from server)@.
  --
  -- Errors on network connection failures or if GitHub sent back an error message. Use `githubTry`
  -- if you wish to handle GitHub errors.
  queryGitHubPage' :: FromJSON a => GHEndpoint -> m (Either (Text, Text) (a, PageLinks))

  -- | 'queryGitHubPage'', except calls 'fail' if JSON decoding fails.
  queryGitHubPage :: FromJSON a => GHEndpoint -> m (a, PageLinks)
  queryGitHubPage = either fail' pure <=< queryGitHubPage'
    where
      fail' (message, response) =
        let ellipses s = if Text.length s > 100 then take 100 (Text.unpack s) ++ "..." else Text.unpack s
        in fail $ "Could not decode response:\nmessage = " ++ ellipses message ++ "\nresponse = " ++ ellipses response

  -- | 'queryGitHubPage', except ignoring pagination links.
  queryGitHub :: FromJSON a => GHEndpoint -> m a
  queryGitHub = fmap fst . queryGitHubPage

  -- | Repeatedly calls 'queryGitHubPage' for each page returned by GitHub and concatenates the
  -- results.
  queryGitHubAll :: (FromJSON a, Monoid a) => GHEndpoint -> m a
  queryGitHubAll ghEndpoint = do
    (payload, pageLinks) <- queryGitHubPage ghEndpoint
    case pageNext pageLinks of
      Just next -> do
        rest <- queryGitHubAll ghEndpoint { endpoint = next, endpointVals = [] }
        return $ payload <> rest
      Nothing -> return payload

  -- | 'queryGitHub', except ignores the result.
  queryGitHub_ :: GHEndpoint -> m ()
  queryGitHub_ = void . queryGitHub @_ @Value

{- Instances for common monad transformers -}

instance MonadGitHubREST m => MonadGitHubREST (ReaderT r m) where
  queryGitHubPage' = lift . queryGitHubPage'

instance MonadGitHubREST m => MonadGitHubREST (ExceptT e m) where
  queryGitHubPage' = lift . queryGitHubPage'

instance MonadGitHubREST m => MonadGitHubREST (IdentityT m) where
  queryGitHubPage' = lift . queryGitHubPage'

instance MonadGitHubREST m => MonadGitHubREST (MaybeT m) where
  queryGitHubPage' = lift . queryGitHubPage'

instance (Monoid w, MonadGitHubREST m) => MonadGitHubREST (Lazy.RWST r w s m) where
  queryGitHubPage' = lift . queryGitHubPage'

instance (Monoid w, MonadGitHubREST m) => MonadGitHubREST (Strict.RWST r w s m) where
  queryGitHubPage' = lift . queryGitHubPage'

instance MonadGitHubREST m => MonadGitHubREST (Lazy.StateT s m) where
  queryGitHubPage' = lift . queryGitHubPage'

instance MonadGitHubREST m => MonadGitHubREST (Strict.StateT s m) where
  queryGitHubPage' = lift . queryGitHubPage'

instance (Monoid w, MonadGitHubREST m) => MonadGitHubREST (Lazy.WriterT w m) where
  queryGitHubPage' = lift . queryGitHubPage'

instance (Monoid w, MonadGitHubREST m) => MonadGitHubREST (Strict.WriterT w m) where
  queryGitHubPage' = lift . queryGitHubPage'
