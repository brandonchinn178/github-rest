{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      :  GitHub.REST.Auth
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Definitions for handling authentication with the GitHub REST API.
-}
module GitHub.REST.Auth (
  Token (..),
  fromToken,

  -- * Helpers for using JWT tokens with the GitHub API
  getJWTToken,
) where

import qualified Crypto.PubKey.RSA as Crypto
import qualified Data.Aeson.Text as Aeson
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as TextL
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Jose.Jwa as Jose
import qualified Jose.Jws as Jose
import qualified Jose.Jwt as Jose
import UnliftIO.Exception (Exception, throwIO)

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
getJWTToken :: Crypto.PrivateKey -> AppId -> IO Token
getJWTToken privKey appId = do
  -- lose a second in the case of rounding
  -- https://github.community/t5/GitHub-API-Development-and/quot-Expiration-time-claim-exp-is-too-far-in-the-future-quot/m-p/20457/highlight/true#M1127
  now <- addUTCTime (-1) <$> getCurrentTime

  BearerToken . Jose.unJwt <$> signToken (mkClaims now)
  where
    mkClaims now =
      Text.encodeUtf8 . TextL.toStrict . Aeson.encodeToLazyText $
        Jose.JwtClaims
          { Jose.jwtIss = Just . Text.pack . show $ appId
          , Jose.jwtSub = Nothing
          , Jose.jwtAud = Nothing
          , Jose.jwtExp = Just . Jose.IntDate $ utcTimeToPOSIXSeconds now + (10 * 60)
          , Jose.jwtNbf = Nothing
          , Jose.jwtIat = Just . Jose.IntDate $ utcTimeToPOSIXSeconds now
          , Jose.jwtJti = Nothing
          }
    signToken claims =
      Jose.rsaEncode Jose.RS256 privKey claims >>= \case
        Right jwt -> pure jwt
        Left e -> throwIO $ JwtError e

-- https://github.com/tekul/jose-jwt/issues/30
data JwtError = JwtError Jose.JwtError
  deriving (Show, Exception)
