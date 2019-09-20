{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PageLinks (tests) where

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck

import GitHub.REST.PageLinks

tests :: [TestTree]
tests =
  [ testProperty "parsePageLinks" $
      forAll genPageLinks $ \(pageFirst, pagePrev, pageNext, pageLast) -> do
        pageLinks <- shuffle $ catMaybes
          [ mkPageLink "first" <$> pageFirst
          , mkPageLink "prev" <$> pagePrev
          , mkPageLink "next" <$> pageNext
          , mkPageLink "last" <$> pageLast
          ]

        return $ parsePageLinks (Text.intercalate ", " pageLinks) === PageLinks{..}
  ]

mkPageLink :: Text -> Text -> Text
mkPageLink rel link = "<https://api.github.com" <> link <> ">; rel=\"" <> rel <> "\""

genPageLinks :: Gen (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
genPageLinks = (,,,) <$> genPageLink <*> genPageLink <*> genPageLink <*> genPageLink
  where
    genPageLink = liftArbitrary $ Text.pack <$> genUrl
    genUrl = ('/' :) <$> listOf (elements urlChars)
    urlChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.~?=+%&/"
