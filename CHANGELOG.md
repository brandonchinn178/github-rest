## Upcoming

* Rename `GitHubState` to `GitHubSettings`
* Remove `queryGitHubPage'` -- implement `queryGitHubPage` in `MonadGitHubREST` instead.

## 1.0.3

* Fix goldens after GitHub changed documentation URL

## 1.0.2

* Remove `MonadFail` constraint on `MonadGitHubREST`
* Support `unliftio-core-0.2.0.0`

## 1.0.1

Bundle test files in release tarball

## 1.0.0

Initial release:

* Implement `queryGitHub` and `GHEndpoint`
* Implement `GitHubT` and `MonadGitHubREST`
