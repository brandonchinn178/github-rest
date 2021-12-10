{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  GitHub.REST.Auth
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for handling authentication with the GitHub REST API.
-}
module GitHub.REST.Auth (
  Token (..),
  fromToken,

  -- * Helpers for using JWT tokens with the GitHub API
  getJWTToken,
  loadSigner,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Web.JWT as JWT

#if MIN_VERSION_jwt(0,11,0)
type EncodeSigner = JWT.EncodeSigner
#else
type EncodeSigner = JWT.Signer
#endif

-- | The token to use to authenticate with GitHub.
data Token
  = -- | https://developer.github.com/v3/#authentication
    AccessToken ByteString
  | -- | https://developer.github.com/apps/building-github-apps/authenticating-with-github-apps/#authenticating-as-a-github-app
    BearerToken ByteString
  deriving (Show)

fromToken :: Token -> ByteString
fromToken = \case
  AccessToken t -> "token " <> t
  BearerToken t -> "bearer " <> t

-- | The ID of your GitHub application
type AppId = Int

-- | Create a JWT token that expires in 10 minutes.
getJWTToken :: EncodeSigner -> AppId -> IO Token
getJWTToken signer appId = mkToken <$> getNow
  where
    mkToken now =
      let claims =
            mempty
              { JWT.iat = JWT.numericDate $ utcTimeToPOSIXSeconds now
              , JWT.exp = JWT.numericDate $ utcTimeToPOSIXSeconds now + (10 * 60)
              , JWT.iss = JWT.stringOrURI $ Text.pack $ show appId
              }
       in BearerToken . Text.encodeUtf8 $ signToken signer claims
    -- lose a second in the case of rounding
    -- https://github.community/t5/GitHub-API-Development-and/quot-Expiration-time-claim-exp-is-too-far-in-the-future-quot/m-p/20457/highlight/true#M1127
    getNow = addUTCTime (-1) <$> getCurrentTime

signToken :: EncodeSigner -> JWT.JWTClaimsSet -> Text
#if MIN_VERSION_jwt(0,10,0)
signToken = flip JWT.encodeSigned mempty
#else
signToken = JWT.encodeSigned
#endif

-- | Load a RSA private key as a Signer from the given file path.
loadSigner :: FilePath -> IO EncodeSigner
loadSigner file = maybe badSigner return . readSigner =<< ByteString.readFile file
  where
    badSigner = fail $ "Not a valid RSA private key file: " ++ file
    readSigner = fmap toEncodeRSAPrivateKey . JWT.readRsaSecret

#if MIN_VERSION_jwt(0,11,0)
    toEncodeRSAPrivateKey = JWT.EncodeRSAPrivateKey
#else
    toEncodeRSAPrivateKey = JWT.RSAPrivateKey
#endif
