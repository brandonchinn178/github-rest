import Test.Tasty (defaultMain, testGroup)

import qualified Endpoint

main :: IO ()
main = defaultMain $ testGroup "github-rest"
  [ testGroup "GitHub.REST.Endpoint" Endpoint.tests
  ]
