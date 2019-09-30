import Test.Tasty (defaultMain, testGroup)

import qualified Endpoint
import qualified Helpers
import qualified MockQuery
import qualified PageLinks

main :: IO ()
main = defaultMain $ testGroup "github-rest"
  [ testGroup "GitHub.REST (Helpers)" Helpers.tests
  , testGroup "GitHub.REST.Endpoint" Endpoint.tests
  , testGroup "GitHub.REST.Monad.Class" MockQuery.tests
  , testGroup "GitHub.REST.PageLinks" PageLinks.tests
  ]
