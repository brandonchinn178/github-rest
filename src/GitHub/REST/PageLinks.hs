{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.REST.PageLinks
  ( PageLinks(..)
  , parsePageLinks
  ) where

import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Types (Header)

-- | Helper type for GitHub pagination.
--
-- https://developer.github.com/v3/guides/traversing-with-pagination/
data PageLinks = PageLinks
  { pageFirst :: Maybe Text
  , pagePrev  :: Maybe Text
  , pageNext  :: Maybe Text
  , pageLast  :: Maybe Text
  } deriving (Eq,Show)

instance Semigroup PageLinks where
  links1 <> links2 = PageLinks
    (pageFirst links1 <> pageFirst links2)
    (pagePrev links1 <> pagePrev links2)
    (pageNext links1 <> pageNext links2)
    (pageLast links1 <> pageLast links2)

instance Monoid PageLinks where
  mempty = PageLinks Nothing Nothing Nothing Nothing

#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

parsePageLinks :: [Header] -> PageLinks
parsePageLinks headers =
  let split delim = map Text.strip . Text.splitOn delim
      dropAround begin end s =
        fromMaybe
          (error $ "Expected value to wrap within " ++ Text.unpack begin ++ "..." ++ Text.unpack end ++ ": " ++ Text.unpack s)
          (Text.stripSuffix end =<< Text.stripPrefix begin s)

      parsePageLink link =
        case split ";" link of
          [url, rel] ->
            let url' = fromMaybe
                  (error $ "Unknown page link: " ++ show link)
                  (Text.stripPrefix ghUrl $ dropAround "<" ">" url)
            in case split "=" rel of
              ["rel", rel'] -> (dropAround "\"" "\"" rel', url')
              _ -> error $ "Unknown page link: " ++ show link
          _ -> error $ "Unknown page link: " ++ show link

      resolve pageLinks "" = pageLinks
      resolve pageLinks link =
        let (rel, url) = parsePageLink link
        in case rel of
          "first" -> pageLinks { pageFirst = Just url }
          "prev" -> pageLinks { pagePrev = Just url }
          "next" -> pageLinks { pageNext = Just url }
          "last" -> pageLinks { pageLast = Just url }
          _ -> error $ "Unknown rel in page link: " ++ show link

      linkHeader = Text.decodeUtf8 . fromMaybe "" . lookup "Link" $ headers

  in foldl resolve mempty . split "," $ linkHeader
  where
    ghUrl = "https://api.github.com"
