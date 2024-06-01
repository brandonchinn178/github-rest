# Unreleased

# v1.2.0

* Switch from `jwt` to `jose-jwt` + `crypton`
    * Removes the `loadSigner` helper, use normal `crypton`/`crypton-x509`/`crypton-x509-store` API
* Add support for GHC 9.8 + 9.10
* Drop support for GHC < 9.6

# v1.1.4

* Fix a test failure due to GitHub changing URLs

# v1.1.3

* Add support for GHC 9.4 + 9.6
* Drop support for GHC < 9
* Set the `X-GitHub-Api-Version` header instead of setting the API version in the `Accept` header ([#33](https://github.com/brandonchinn178/github-rest/issues/33))

# v1.1.2

* Add support for `jwt-0.11.0`

# v1.1.1

* Add support for `aeson-2.0.0.0`

# v1.1.0

* Rename `GitHubState` to `GitHubSettings`
* Remove `queryGitHubPage'` -- implement `queryGitHubPage` in `MonadGitHubREST` instead.
* Expose `queryGitHubPageIO` if users want to manually implement `MonadGitHubREST`
* Add `DecodeError` error

# v1.0.3

* Fix goldens after GitHub changed documentation URL

# v1.0.2

* Remove `MonadFail` constraint on `MonadGitHubREST`
* Support `unliftio-core-0.2.0.0`

# v1.0.1

Bundle test files in release tarball

# v1.0.0

Initial release:

* Implement `queryGitHub` and `GHEndpoint`
* Implement `GitHubT` and `MonadGitHubREST`
