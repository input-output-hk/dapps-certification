{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Instances.Persistence where

import Test.QuickCheck
import IOHK.Certification.Persistence
import Plutus.Certification.API
import Control.Monad (replicateM)
import Data.Text
import GHC.TypeLits (KnownSymbol)
import Debug.Trace

--------------------------------------------------------------------------------
-- | GitHubAccessToken

genGHSuffix :: Gen Text
genGHSuffix = pack <$> replicateM 36 (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

instance Arbitrary GitHubAccessTokenType where
  arbitrary = elements [OAuthToken, UserToServerToken, ServerToServerToken, RefreshToken]

instance Arbitrary GitHubAccessToken where
  arbitrary = GitHubAccessToken
    <$> arbitrary
    <*> genGHSuffix

-- between 1 and 15 characters, alphanumeric and underscore
genTwitterAccount :: Gen Text
genTwitterAccount = do
  length' <- choose (1, 15)
  pack <$> replicateM length' (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_")


instance Arbitrary Twitter where
  arbitrary = genPatternedTextWith genTwitterAccount

genPatternedTextWith :: (KnownSymbol n, KnownSymbol p) => Gen Text -> Gen (PatternedText n p)
genPatternedTextWith g = do
  text' <- g
  case mkPatternedText text' of
    Right b -> pure b
    Left s -> traceError "genPatternedTextWith" s (show text')

traceError :: [Char] -> [Char] -> [Char] -> a
traceError propS err info =
  trace ("\n\tERROR "++ propS ++ ":\n\t" ++ err ++ "\n" ++ info ++ "\n\n\n") (error "error")

genCardanoAddress :: Gen Text
genCardanoAddress = do
  prefix <- elements ["addr_test1", "addr1", "stake", "stake_test1"]
  length' <- choose (53, 98)
  address' <- replicateM length' (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
  return $ pack $ prefix ++ address'

instance Arbitrary ProfileWalletAddress where
  arbitrary = genPatternedTextWith genCardanoAddress

genWebsite :: Gen Text
genWebsite = do
  prefix <- elements ["https://", "http://"]

  domain <- genFullDomain

  partsLen :: Int <- choose (1,2)
  parts  <- replicateM partsLen genPart
  let path = Prelude.foldl (\acc x -> acc ++ "/" ++ x) "/" parts
  queryParamsLen :: Int <- choose (0,10)
  queryParams <- replicateM queryParamsLen genQueryParam
  let queryParamsStr = Prelude.foldl (\acc x -> acc ++ "&" ++ x) "?" queryParams
  return $ pack $ prefix ++ domain ++ path ++ "?" ++ queryParamsStr
  where

  genFullDomain = do
    len <- choose (1, 4)
    subDomains <- replicateM len genSubDomain
    return $ Prelude.foldl aggregate "" subDomains  ++ ".com"
    where
    aggregate "" x = x
    aggregate acc y = acc ++ "." ++ y

  genSubDomain = do
    len <- choose (1, 4)
    replicateM len (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

  genPart = do
    len <- choose (1, 15)
    replicateM len (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._")
  genQueryParam = do
    paramNameLen <- choose (1, 15)
    paramName <- replicateM paramNameLen (elements alphaNum)
    paramValueLen <- choose (1, 15)
    paramValue <- replicateM paramValueLen (elements alphaNum)
    return $ paramName ++ "=" ++ paramValue

alphaNum :: [Char]
alphaNum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

instance Arbitrary Website where
  arbitrary = genPatternedTextWith genWebsite

instance Arbitrary LinkedIn where
  arbitrary = genPatternedTextWith genLinkedIn

instance Arbitrary Subject where
  arbitrary = genPatternedTextWith genSubject

-- LinkedIn regex:
-- ^(http(s)?:\/\/)?([\w]+\.)?linkedin\.com\/(pub|in|profile|company)\/([a-zA-Z0-9_-]+)$

genLinkedIn :: Gen Text
genLinkedIn = do
  prefix <- elements ["https://", "http://"]
  domain <- elements ["linkedin.com", "www.linkedin.com"]
  suffix <- elements ["/pub/", "/in/", "/profile/", "/company/"]
  account <- genLinkedInAccount
  return $ pack $ prefix ++ domain ++ suffix ++ account
  where
  genLinkedInAccount = do
    len <- choose (1, 15)
    replicateM len (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

genSubject :: Gen Text
genSubject = do
  len <- choose (1, 64)
  pack <$> replicateM len (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

instance Arbitrary Email where
  arbitrary = genPatternedTextWith genEmail

-- Email regex:
-- "^\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}$"

genEmail :: Gen Text
genEmail = do
  prefix <- replicateM 10 (elements (alphaNum ++ "_."))
  domain <- replicateM 10 (elements (alphaNum ++ "_."))
  suffix <- replicateM 3 (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
  return $ pack $ prefix ++ "@" ++ domain ++ "." ++ suffix


genArbitraryName :: Gen Text
genArbitraryName = do
  len <- choose (1, 15)
  pack <$> replicateM len (elements (alphaNum ++ "-_"))

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = do
  b <- arbitrary
  if b then Just <$> g else return Nothing

instance Arbitrary ProfileId where
  arbitrary = toId . (+1) . abs <$> arbitrary

instance Arbitrary Profile where
  arbitrary = Profile (toId (-1))
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> genMaybe genArbitraryName
    <*> genMaybe genArbitraryName

instance Arbitrary DApp where
  arbitrary = DApp (toId (-1))
    <$> genArbitraryName
    <*> genArbitraryName
    <*> genArbitraryName
    <*> genArbitraryName
    <*> arbitrary
    <*> arbitrary

instance Arbitrary DAppDTO where
  arbitrary = DAppDTO <$> arbitrary

instance Arbitrary ProfileDTO where
  arbitrary = ProfileDTO
    <$> arbitrary
    <*> arbitrary

instance Arbitrary ProfileBody where
  arbitrary = ProfileBody <$> arbitrary <*> arbitrary
