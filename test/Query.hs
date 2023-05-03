{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Query (tests) where

import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text.Lazy.Encoding as Text
import Network.HTTP.Types (status404)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Golden (goldenVsString)

import GitHub.REST

tests :: [TestTree]
tests =
  [ goldens "Query Gist #1" "gists-gist_id-sha.golden" $ do
      gist <- queryGitHub $ getGist 1 "8afc134bf14f8f56ed2e7234128490d9946e8c16"
      return $ Text.encodeUtf8 $ gist .: "files" .: "gistfile1.txt" .: "content"
  , goldens "Query non-existent Gist commit" "gists-gist_id-sha-422.golden" $ do
      showResult . githubTry . queryGitHub $
        getGist 1 "d00770679ba293a327156b9e7031c47c6d269157"
  , goldens "Query non-existent Gist" "gists-gist_id-sha-404.golden" $ do
      showResult . githubTry' status404 . queryGitHub $
        getGist 0 "d00770679ba293a327156b9e7031c47c6d269157"
  ]

goldens :: TestName -> String -> GitHubT IO ByteString -> TestTree
goldens name fp action = goldenVsString name ("test/goldens/" ++ fp) $ runGitHubT state action
  where
    state =
      GitHubSettings
        { token = Nothing
        , userAgent = "github-rest"
        , apiVersion = ""
        }

showResult :: (Monad m) => m (Either Value Value) -> m ByteString
showResult m =
  m >>= \case
    Right v -> error $ "Got back invalid result: " ++ show v
    Left e -> return $ Char8.pack $ show e

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
