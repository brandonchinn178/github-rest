# github-rest

[![GitHub Actions](https://img.shields.io/github/actions/workflow/status/brandonchinn178/github-rest/ci.yml?branch=main)](https://github.com/brandonchinn178/github-rest/actions?query=branch%3Amain)
[![Codecov](https://codecov.io/gh/brandonchinn178/github-rest/branch/main/graph/badge.svg?token=8TErU2ntw9)](https://codecov.io/gh/brandonchinn178/github-rest)
[![Hackage](https://img.shields.io/hackage/v/github-rest)](https://hackage.haskell.org/package/github-rest)

A package providing a more flexible interface to accessing the [GitHub API](https://developer.github.com/v3/).
Endpoints are created using the `GHEndpoint` constructor and are executed with
the `queryGitHub` function in the `GitHubT` monad.

## Quickstart

This quickstart will demonstrate querying endpoints in a hypothetical public
GitHub repo `alice/my-project`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Text (Text)
import GitHub.REST
import Network.HTTP.Types (StdMethod(..))

default (Text)

main = do
  let state = GitHubSettings
        { token = Nothing
          -- ^ An authentication token to use, if any.
        , userAgent = "alice/my-project"
          -- ^ GitHub requires this to be set to a User Agent specific to your
          -- application: https://developer.github.com/v3/#user-agent-required
        , apiVersion = "v3"
          -- ^ Specifies the API version to query: https://developer.github.com/v3/media/
        }

  runGitHubT state $ do

    -- Get information for the "main" branch
    -- https://developer.github.com/v3/git/refs/#get-a-single-reference
    ref <- queryGitHub GHEndpoint
      { method = GET
        -- Colon-prefixed components in the endpoint will be interpolated by
        -- the values in 'endpointVals'.
        -- In this case, "/repos/alice/my-project/git/refs/heads/main"
      , endpoint = "/repos/:owner/:repo/git/refs/:ref"
      , endpointVals =
        [ "owner" := "alice"
        , "repo" := "my-project"
        , "ref" := "heads/main"
        ]
      , ghData = []
      }

    -- 'github-rest' provides a '.:' helper for when the API guarantees that a
    -- key in a JSON object exists
    --
    -- The result of 'queryGitHub' is anything that's an instance of FromJSON,
    -- if using manually-defined data types is preferred over using '.:'. This
    -- package can be easily used with the aeson-schemas library, which
    -- provides a type-safe way to query JSON data.
    let sha :: Text
        sha = ref .: "object" .: "sha"

    -- Create a new branch called "foo"
    -- https://developer.github.com/v3/git/refs/#create-a-reference
    queryGitHub GHEndpoint
      { method = POST
      , endpoint = "/repos/:owner/:repo/git/refs"
      , endpointVals =
        [ "owner" := "alice"
        , "repo" := "my-project"
        ]
      , ghData =
        [ "ref" := "refs/heads/foo"
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

* `github-rest` passes authentication once, with requests executed in a single
  monadic context. The `github` package requires passing in an authentication
  token every time a request is executed

* In the same vein, `github-rest` provides a monad transformer that handles all
  GitHub state needed to execute `queryGitHub`. `github` runs everything in
  `IO`, expecting the caller to keep track of GitHub state manually.

* `github-rest` allows usage with [`aeson-schemas`](http://hackage.haskell.org/package/aeson-schemas)
