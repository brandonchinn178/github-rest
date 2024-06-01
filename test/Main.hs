import Test.Tasty (defaultMain, testGroup)

import qualified Auth
import qualified Endpoint
import qualified Helpers
import qualified MockQuery
import qualified PageLinks
import qualified Query

main :: IO ()
main =
  defaultMain $
    testGroup
      "github-rest"
      [ testGroup "GitHub.REST (Helpers)" Helpers.tests
      , testGroup "GitHub.REST.Endpoint" Endpoint.tests
      , testGroup "GitHub.REST.Monad.Class" MockQuery.tests
      , testGroup "GitHub.REST.PageLinks" PageLinks.tests
      , testGroup "GitHub.REST.Auth" Auth.tests
      , testGroup "GitHub.REST (End-to-End)" Query.tests
      ]
