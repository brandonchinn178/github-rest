{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.REST.PageLinks (
  PageLinks (..),
  parsePageLinks,
) where

import Data.Maybe (fromMaybe)

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Text (Text)
import qualified Data.Text as Text

-- | Helper type for GitHub pagination.
--
--  https://developer.github.com/v3/guides/traversing-with-pagination/
data PageLinks = PageLinks
  { pageFirst :: Maybe Text
  , pagePrev :: Maybe Text
  , pageNext :: Maybe Text
  , pageLast :: Maybe Text
  }
  deriving (Eq, Show)

instance Semigroup PageLinks where
  links1 <> links2 =
    PageLinks
      (pageFirst links1 <> pageFirst links2)
      (pagePrev links1 <> pagePrev links2)
      (pageNext links1 <> pageNext links2)
      (pageLast links1 <> pageLast links2)

instance Monoid PageLinks where
  mempty = PageLinks Nothing Nothing Nothing Nothing

#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

parsePageLinks :: Text -> PageLinks
parsePageLinks = foldl resolve mempty . split ","
  where
    resolve :: PageLinks -> Text -> PageLinks
    resolve pageLinks "" = pageLinks
    resolve pageLinks link =
      let (rel, url) = parsePageLink link
       in case rel of
            "first" -> pageLinks{pageFirst = Just url}
            "prev" -> pageLinks{pagePrev = Just url}
            "next" -> pageLinks{pageNext = Just url}
            "last" -> pageLinks{pageLast = Just url}
            _ -> error $ "Unknown rel in page link: " ++ show link

-- | Parse a single page link, like:
--
--  <https://api.github.com/search/code?q=addClass+user%3Amozilla&page=2>; rel="next"
--
--  Returns ("next", "/search/code?q=addClass+user%3Amozilla&page=2")
parsePageLink :: Text -> (Text, Text)
parsePageLink link = fromMaybe (error $ "Unknown page link: " ++ show link) $ do
  (linkUrl, linkRel) <- case split ";" link of
    [url, rel] -> pure (url, rel)
    _ -> mempty

  url <- Text.stripPrefix ghUrl $ dropAround "<" ">" linkUrl
  rel <- case split "=" linkRel of
    ["rel", linkRel'] -> pure $ dropAround "\"" "\"" linkRel'
    _ -> mempty

  pure (rel, url)
  where
    ghUrl = "https://api.github.com"

{- Helpers -}

-- | Split the given text by the given delimiter, stripping any surrounding whitespace.
split :: Text -> Text -> [Text]
split delim = map Text.strip . Text.splitOn delim

-- | Drop the given strings at the beginning and end of the given text.
dropAround :: Text -> Text -> Text -> Text
dropAround begin end s = fromMaybe badDrop $ Text.stripSuffix end =<< Text.stripPrefix begin s
  where
    badDrop = error $ "Expected value to wrap within " ++ Text.unpack begin ++ "..." ++ Text.unpack end ++ ": " ++ Text.unpack s
