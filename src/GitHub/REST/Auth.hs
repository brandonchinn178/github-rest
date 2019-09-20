{-|
Module      :  GitHub.REST.Auth
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for handling authentication with the GitHub REST API.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.REST.Auth
  ( Token(..)
  , fromToken
  -- * Helpers for using JWT tokens with the GitHub API
  , getJWTToken
  , loadSigner
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Web.JWT as JWT

-- | The token to use to authenticate with GitHub.
data Token
  = AccessToken ByteString
    -- ^ https://developer.github.com/v3/#authentication
  | BearerToken ByteString
    -- ^ https://developer.github.com/apps/building-github-apps/authenticating-with-github-apps/#authenticating-as-a-github-app
  deriving (Show)

fromToken :: Token -> ByteString
fromToken = \case
  AccessToken t -> "token " <> t
  BearerToken t -> "bearer " <> t

-- | Create a JWT token that expires in 10 minutes.
getJWTToken :: JWT.Signer -> Int -> IO Token
getJWTToken signer appId = mkToken <$> getNow
  where
    mkToken now =
      let claims = mempty
            { JWT.iat = JWT.numericDate $ utcTimeToPOSIXSeconds now
            , JWT.exp = JWT.numericDate $ utcTimeToPOSIXSeconds now + (10 * 60)
            , JWT.iss = JWT.stringOrURI $ Text.pack $ show appId
            }
      in BearerToken . Text.encodeUtf8 $ JWT.encodeSigned signer claims
    -- lose a second in the case of rounding
    -- https://github.community/t5/GitHub-API-Development-and/quot-Expiration-time-claim-exp-is-too-far-in-the-future-quot/m-p/20457/highlight/true#M1127
    getNow = addUTCTime (-1) <$> getCurrentTime

-- | Load a RSA private key as a Signer from the given file path.
loadSigner :: FilePath -> IO JWT.Signer
loadSigner file = maybe badSigner return . readSigner =<< ByteString.readFile file
  where
    badSigner = fail $ "Not a valid RSA private key file: " ++ file
    readSigner = fmap JWT.RSAPrivateKey . JWT.readRsaSecret
