{-# LANGUAGE QuasiQuotes #-}

module Helpers (tests) where

import Data.Aeson.QQ (aesonQQ)
import qualified Data.Text as Text
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck

import GitHub.REST ((.:))

tests :: [TestTree]
tests =
  [ testProperty "o .: key" $ \(PrintableString key) (PrintableString val) ->
      [aesonQQ| { $key: #{val} } |] .: Text.pack key === val
  ]
