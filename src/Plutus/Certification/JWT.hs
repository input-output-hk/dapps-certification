{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Plutus.Certification.JWT
  ( jwtEncode
  ,jwtDecode
  , JWTError(..)
  , JWTArgs(..)
  ) where

import           Data.Aeson            (FromJSON (..), Result (..), ToJSON (..),
                                        fromJSON)
import           Data.Map              as Map
import           Data.Text             as T
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)
import           Web.JWT               as JWT

import Prelude hiding (exp)

data JWTArgs = JWTArgs
  { jwtSecret :: !String
  , jwtExpirationSeconds :: !Integer
  }

defaultKey :: Text
defaultKey = "default"

jwtEncode :: ToJSON p => String -> UTCTime -> p -> Text
jwtEncode secret time a =
  let cs = mempty -- mempty returns a default JWTClaimsSet
         { unregisteredClaims = ClaimsMap $ Map.fromList [(defaultKey, toJSON a)]
         , JWT.exp =  numericDate (utcTimeToPOSIXSeconds time) -- 0 means no expiry
         }
      key = hmacSecret . T.pack $ secret
  in encodeSigned key mempty cs

data JWTError = JWTDecodingFailure
              | JWTSigVerificationFailure
              | JWTClaimsVerificationFailure
              | JWTDefaultKeyDecodingFailure String
              | JWTExpirationMissing
              deriving (Show, Eq)

jwtDecode :: FromJSON b => String -> Text -> Either JWTError (b,UTCTime)
jwtDecode secret input = let
     verify' = verify (toVerify . hmacSecret . T.pack $ secret)
 in case JWT.decode input of
     Nothing -> Left JWTDecodingFailure
     Just unverified -> case verify' unverified of
        Nothing -> Left JWTSigVerificationFailure
        Just verified | JWTClaimsSet{..} <- claims verified ->
          case Map.lookup defaultKey (unClaimsMap unregisteredClaims) of
            Nothing -> Left JWTClaimsVerificationFailure
            Just value -> case (exp,fromJSON value) of
             (_,Error e) -> Left $ JWTDefaultKeyDecodingFailure e
             (Nothing,_) -> Left JWTExpirationMissing
             (Just exp',Success a) -> Right (a, posixSecondsToUTCTime $ secondsSinceEpoch exp')

-- >>> jwtEncode "my-secret" (posixSecondsToUTCTime 0) ()
-- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkZWZhdWx0IjpbXSwiZXhwIjowfQ.y0cPrykhJraVZcfGZ5JHoG2d41unSUuplI1m1hOoDgA"

-- >>> JWT.decode $ jwtEncode "my-secret" (posixSecondsToUTCTime 9999) ()
-- Just (Unverified (JOSEHeader {typ = Just "JWT", cty = Nothing, alg = Just HS256, kid = Nothing}) (JWTClaimsSet {iss = Nothing, sub = Nothing, aud = Nothing, exp = Just (NumericDate 9999), nbf = Nothing, iat = Nothing, jti = Nothing, unregisteredClaims = ClaimsMap {unClaimsMap = fromList [("default",Array [])]}}) (Signature "SoalNhOCBlJEOR6asfwKZ1bA-p0NaEdS-zdoVxVcuIo") "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkZWZhdWx0IjpbXSwiZXhwIjo5OTk5fQ")

-- >>> jwtDecode @Text "my-secret" $ jwtEncode "my-secret" (posixSecondsToUTCTime 0) ( "My-payload" :: String)
-- Right ("My-payload",1970-01-01 00:00:00 UTC)

-- >>> jwtDecode @Int "my-secret" $ jwtEncode "my-secret" (posixSecondsToUTCTime 0) True
-- Left (JWTDefaultKeyDecodingFailure "parsing Int failed, expected Number, but encountered Boolean")

-- >>> jwtDecode "sadas" $ jwtEncode "my-secret" (posixSecondsToUTCTime 0) "ASDADASDAS"
-- Left JWTSigVerificationFailure
