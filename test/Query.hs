{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Query (tests) where

import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy.Encoding as Text
import Network.HTTP.Types (status404)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

import GitHub.REST

tests :: [TestTree]
tests =
  [ goldens "Query Gist #1" "gists-gist_id-sha.golden" $ do
      gist <- queryGitHub $ getGist 1 "8afc134bf14f8f56ed2e7234128490d9946e8c16"
      return $ Text.encodeUtf8 $ gist .: "files" .: "gistfile1.txt" .: "content"
  , testCase "Query non-existent Gist commit" $ do
      (Left e :: Either Value Value) <-
        runGitHubT ghSettings . githubTry . queryGitHub $
          getGist 1 "d00770679ba293a327156b9e7031c47c6d269157"
      e .: "message" @?= ("No commit found for SHA: d00770679ba293a327156b9e7031c47c6d269157" :: String)
  , testCase "Query non-existent Gist" $ do
      (Left e :: Either Value Value) <-
        runGitHubT ghSettings . githubTry' status404 . queryGitHub $
          getGist 0 "d00770679ba293a327156b9e7031c47c6d269157"
      e .: "message" @?= ("Not Found" :: String)
  ]

goldens :: TestName -> String -> GitHubT IO ByteString -> TestTree
goldens name fp action = goldenVsString name ("test/goldens/" ++ fp) $ runGitHubT ghSettings action

ghSettings :: GitHubSettings
ghSettings =
  GitHubSettings
    { token = Nothing
    , userAgent = "github-rest"
    , apiVersion = ""
    }

getGist :: Int -> String -> GHEndpoint
getGist gistId gistSha =
  GHEndpoint
    { method = GET
    , endpoint = "/gists/:gist_id/:sha"
    , endpointVals =
        [ "gist_id" := gistId
        , "sha" := gistSha
        ]
    , ghData = []
    }
