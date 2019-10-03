# github-rest

A package providing a more flexible interface to accessing the GitHub API.
Endpoints are created using the `GHEndpoint` constructor and are executed with
the `queryGitHub` function in the `GitHubT` monad.

## Quickstart

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Text (Text)
import GitHub.REST
import Network.HTTP.Types (StdMethod(..))

main = do
  let repoOwner = "alice"
      repoName = "my-project"
      state = GitHubState
        { token = Nothing
        , userAgent = repoOwner <> "/" <> repoName
        , apiVersion = "v3"
        }

  runGitHubT state $ do
    ref <- queryGitHub GHEndpoint
      { method = GET
      , endpoint = "/repos/:owner/:repo/git/refs/:ref"
      , endpointVals =
        [ "owner" := repoOwner
        , "repo" := repoName
        , "ref" := ("heads/master" :: Text)
        ]
      , ghData = []
      }

    let sha = ref .: "object" .: "sha" :: Text

    queryGitHub GHEndpoint
      { method = POST
      , endpoint = "/repos/:owner/:repo/git/refs"
      , endpointVals =
        [ "owner" := repoOwner
        , "repo" := repoName
        ]
      , ghData =
        [ "ref" := ("refs/heads/foo" :: Text)
        , "sha" := sha
        ]
      }
```

## Comparison to other libraries

The `github` package provides a decent API for querying the GitHub API,
and it defines Haskell data types for each endpoint. These data types can
be used as the result of `queryGitHub`. 

This package provides a different interface for people with different tastes:

* `github-rest` informs the user exactly which GitHub endpoint is being hit
  (e.g. `/repos/:owner/:repo`). Users no longer need to spend time trying to 
  scour documentation to find the corresponding function for an endpoint.

* `github-rest` passes authentication once, with requests executed in 
  one monadic context. The `github` package requires passing in an authentication
  token every time a request is executed

* In the same vein, `github-rest` provides a monad transformer that handles all GitHub
  state needed to execute `queryGitHub`. `github` runs everything in `IO`,
  expecting the caller to keep track of GitHub state manually.

* `github-rest` allows usage with [`aeson-schemas`](http://hackage.haskell.org/package/aeson-schemas)
