{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module MockQuery (tests) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Data.Aeson (Result(..), Value(..), fromJSON)
import Data.Either (isLeft)
import Data.List (uncons)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import GHC.Exts (fromList)
import Network.HTTP.Types (StdMethod(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import GitHub.REST.Endpoint (GHEndpoint(..))
import GitHub.REST.Monad.Class
import GitHub.REST.PageLinks (PageLinks(..))

tests :: [TestTree]
tests =
  [ testGroup "queryGitHubPage"
    [ testCase "success single" $ do
        (result, pageLinks) <- runMockREST (queryGitHubPage @_ @Text) [Right "a"]
        result @?= "a"
        isNothing (pageNext pageLinks) @? "PageLinks 'next' rel is not Nothing"
    , testCase "success multiple" $ do
        (result, pageLinks) <- runMockREST (queryGitHubPage @_ @Text) [Right "a", Right "b"]
        result @?= "a"
        isJust (pageNext pageLinks) @? "PageLinks 'next' rel is Nothing"
    , testCase "error single" $ do
        result <- try @SomeException $ runMockREST (queryGitHubPage @_ @Text) [Left "error message!"]
        isLeft result @? "query did not error"
    , testCase "success with error in second page" $ do
        (result, pageLinks) <- runMockREST (queryGitHubPage @_ @Text) [Right "a", Left "error message!"]
        result @?= "a"
        isJust (pageNext pageLinks) @? "PageLinks 'next' rel is Nothing"
    ]

  , testGroup "queryGitHub"
    [ testCase "success single" $ do
        result <- runMockREST (queryGitHub @_ @Text) [Right "a"]
        result @?= "a"
    , testCase "success multiple" $ do
        result <- runMockREST (queryGitHub @_ @Text) [Right "a", Right "b"]
        result @?= "a"
    , testCase "error single" $ do
        result <- try @SomeException $ runMockREST (queryGitHub @_ @Text) [Left "error message!"]
        isLeft result @? "query did not error"
    , testCase "success with error in second page" $ do
        result <- runMockREST (queryGitHub @_ @Text) [Right "a", Left "error message!"]
        result @?= "a"
    ]

  , testGroup "queryGitHubAll"
    [ testCase "success single" $ do
        result <- runMockREST (queryGitHubAll @_ @[Text]) [Right "a"]
        result @?= ["a"]
    , testCase "success multiple" $ do
        result <- runMockREST (queryGitHubAll @_ @[Text]) [Right "a", Right "b"]
        result @?= ["a", "b"]
    , testCase "error single" $ do
        result <- try @SomeException $ runMockREST (queryGitHubAll @_ @[Text]) [Left "error message!"]
        isLeft result @? "query did not error"
    , testCase "error in second page" $ do
        result <- try @SomeException $ runMockREST (queryGitHubAll @_ @[Text]) [Right "a", Left "error message!"]
        isLeft result @? "query did not error"
    ]

  , testGroup "queryGitHub_"
    [ testCase "success single" $
        runMockREST queryGitHub_ [Right "a"]
    , testCase "success multiple" $
        runMockREST queryGitHub_ [Right "a", Right "b"]
    , testCase "error single" $ do
        result <- try @SomeException $ runMockREST queryGitHub_ [Left "error message!"]
        isLeft result @? "query did not error"
    , testCase "success with error in second page" $
        runMockREST queryGitHub_ [Right "a", Left "error message!"]
    ]
  ]

{- Mock implementation -}

newtype MockREST a = MockREST { unMockREST :: StateT [Either Text Text] IO a }
  deriving (Functor,Applicative,Monad)

runMockREST :: (GHEndpoint -> MockREST a) -> [Either Text Text] -> IO a
runMockREST f results = (`evalStateT` results) $ unMockREST $ f ghEndpoint
  where
    ghEndpoint = GHEndpoint
      { method = GET
      , endpoint = "/"
      , endpointVals = []
      , ghData = []
      }

instance MonadGitHubREST MockREST where
  queryGitHubPage' _ = do
    (curr, rest) <- fromMaybe (error "Did you forget to mock a query?") . uncons <$> MockREST get
    MockREST $ put rest

    case curr of
      Left e -> return $ Left (e, e)
      Right v -> do
        result <- case fromJSON (String v) <|> fromJSON (Array $ fromList [String v]) of
          Success a -> return a
          Error _ -> error "MockREST only supports returning Text or [Text]"

        let pageLinks = case rest of
              [] -> mempty
              _ -> mempty { pageNext = Just "/" }

        return $ Right (result, pageLinks)
