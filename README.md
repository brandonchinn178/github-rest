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

The `github` package provides a pretty good API for querying the GitHub API,
and it defines Haskell data types for each endpoint. If you want, you can
definitely use those data types as the result of `queryGitHub`. This package
provides a different interface for people with different tastes:

* I personally would rather know exactly which GitHub endpoint I'm hitting
  (e.g. `/repos/:owner/:repo`) than trying to scour the documentation to find
  the function I want

* It seems like you have to pass in your authentication everytime you execute
  a request with the `github` package, whereas in `github-rest`, authentication
  is passed once, with requests executed in one monadic context

* Relatedly, `github-rest` provides a monad transformer that handles all GitHub
  state needed to execute `queryGitHub`

* `github-rest` allows usage with [`aeson-schemas`](http://hackage.haskell.org/package/aeson-schemas)
