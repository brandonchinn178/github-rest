{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Endpoint (tests) where

import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Semigroup (Semigroup)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Types (StdMethod(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck

import GitHub.REST.Endpoint
import GitHub.REST.KeyValue (KeyValue(..))

tests :: [TestTree]
tests =
  [ testProperty "endpointPath with no substitutions" $
      forAll (listOf genNoSubstitute) $ \endpointComponents ->
        let ghEndpoint = mkGHEndpoint endpointComponents []
        in endpointPath ghEndpoint === concatComponents endpointComponents

  , testProperty "endpointPath with substitutions" $
      forAll (listOf genNoSubstitute) $ \withoutColons ->
        forAll (listOf1 genSubstitute) $ \withColonAndVals' -> do
          -- we don't support duplicate substitutions
          let withColonAndVals = nubBy ((==) `on` fst) withColonAndVals'

          -- list of components before/after substituting
          let allComponents = map (\ec -> (ec, ec)) withoutColons ++ withColonAndVals
          (before, after) <- unzip <$> shuffle allComponents

          let ghEndpoint = mkGHEndpoint before $ map fromSubstitutePair withColonAndVals

          pure $ endpointPath ghEndpoint === concatComponents after
  ]

{- Helpers -}

mkGHEndpoint :: [EndpointComponent] -> EndpointVals -> GHEndpoint
mkGHEndpoint endpointComponents endpointVals = GHEndpoint
  { method = GET
  , endpoint = concatComponents endpointComponents
  , endpointVals
  , ghData = []
  }

concatComponents :: [EndpointComponent] -> Text
concatComponents = ("/" <>) . Text.intercalate "/" . map unComponent

-- | A component in an endpoint path; e.g. does not contain forward slashes.
newtype EndpointComponent = EndpointComponent { unComponent :: Text }
  deriving (Show,Eq,IsString,Monoid,Semigroup)

-- | Generate a non-substitutionable EndpointComponent.
genNoSubstitute :: Gen EndpointComponent
genNoSubstitute = fmap fromString $ listOf1 $ elements allowedUrlCharacters
  where
    -- https://stackoverflow.com/a/41353282
    allowedUrlCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.~"

-- | Generate a substitutionable EndpointComponent and a value to interpolate.
genSubstitute :: Gen (EndpointComponent, EndpointComponent)
genSubstitute = do
  component <- genNoSubstitute
  val <- genNoSubstitute
  pure (":" <> component, val)

fromSubstitutePair :: (EndpointComponent, EndpointComponent) -> KeyValue
fromSubstitutePair (EndpointComponent key, EndpointComponent val) = key' := val
  where
    key' = fromMaybe key $ Text.stripPrefix ":" key
