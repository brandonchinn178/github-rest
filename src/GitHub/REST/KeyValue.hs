{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

{-|
Module      :  GitHub.REST.KeyValue
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Define the 'KeyValue' helper type.
-}
module GitHub.REST.KeyValue (
  KeyValue (..),
  kvToValue,
  kvToText,
) where

import Data.Aeson (ToJSON (..), Value (..), object)
import Data.Aeson.Types (Pair)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as Text

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#endif

-- | A type representing a key-value pair.
data KeyValue where
  (:=) :: (Show v, ToJSON v) => Text -> v -> KeyValue

infixr 1 :=

instance Show KeyValue where
  show = show . kvToText

instance {-# OVERLAPS #-} ToJSON [KeyValue] where
  toJSON = kvToValue

-- | Convert a 'KeyValue' into a 'Pair'.
toPair :: KeyValue -> Pair
toPair (k := v) = (fromText k, toJSON v)

-- | Convert the given KeyValues into a JSON Object.
kvToValue :: [KeyValue] -> Value
kvToValue = object . map toPair

-- | Represent the given KeyValue as a pair of Texts.
kvToText :: KeyValue -> (Text, Text)
kvToText (k := v) = (k, v')
  where
    v' = case toJSON v of
      String t -> t
      Number x -> Text.pack . prettyNum $ x
      Bool b -> Text.pack . show $ b
      _ -> error $ "Could not convert value: " ++ show v
    prettyNum x = either show show (floatingOrInteger x :: Either Double Integer)

{- Helpers -}

#if !MIN_VERSION_aeson(2,0,0)
fromText :: Text -> Text
fromText = id
#endif
