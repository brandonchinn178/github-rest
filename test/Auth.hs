module Auth (tests) where

import qualified Crypto.PubKey.RSA as Crypto
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import GitHub.REST.Auth

tests :: [TestTree]
tests =
  [ testCase "getJWTToken works" $ do
      (_, privKey) <- Crypto.generate 256 3
      _ <- getJWTToken privKey 123
      pure ()
  ]
