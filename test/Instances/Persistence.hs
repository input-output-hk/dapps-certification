{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedRecordDot   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Instances.Persistence where

import Test.QuickCheck
import IOHK.Certification.Persistence as DB
import Plutus.Certification.API
import Plutus.Certification.Metadata
import Control.Monad (replicateM)
import Data.Text
import GHC.TypeLits (KnownSymbol)
import Debug.Trace
import Test.QuickCheck.Instances.Time ()
import Plutus.Certification.ProfileWallet as PW

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
    where
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

instance Arbitrary Subject where
  arbitrary = genPatternedTextWith genSubject
    where
    genSubject :: Gen Text
    genSubject = do
      len <- choose (1, 64)
      pack <$> replicateM len (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")


instance Arbitrary GitHubAccount where
  arbitrary = genPatternedTextWith genGitHubAccount
    where
    genGitHubAccount = do
      hasHyphen :: Bool <- arbitrary
      len <- choose (5, if hasHyphen then 38 else 39)
      whereToSplit <- choose (2, len-1)
      prefix <- replicateM whereToSplit (elements alphaNum)
      suffix <- replicateM (len - whereToSplit) (elements alphaNum)
      return $ pack $ prefix ++ (if hasHyphen then "-" else "") ++ suffix

-- Pattern for Discord ^(?:https?:\/\/)?discord(?:\.gg|app\.com\/invite|\.com\/invite)\/[\w-]+$
-- Valid examples:
-- https://discordapp.com/invite/asda
-- https://discord.com/invite/asda
-- https://discord.gg/asda
instance Arbitrary DiscordLink where
  arbitrary = genPatternedTextWith discordLink
    where
    discordLink = do
      prefix <- elements ["https://", "http://"]
      domain <- elements ["discordapp.com", "discord.com", "discord.gg"]
      invite <- elements ["/invite/"]
      account <- genDiscordAccount
      return $ pack $ prefix ++ domain ++ (if domain == "discord.gg" then "/" else invite) ++ account
      where
      genDiscordAccount = do
        len <- choose (1, 15)
        replicateM len (elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

instance Arbitrary Social where
  arbitrary = Social
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary CertificationIssuerName where
  arbitrary = genPatternedTextWith genArbitraryIssuerName
    where
    -- generate for "^.{1,64}$"
    genArbitraryIssuerName = do
      len <- choose (1, 64)
      -- generate a printable ASCII char
      let printableChar = elements [' '..'~']
      -- any chars not only the alphanumeric ones
      pack <$> replicateM len printableChar

instance Arbitrary CertificateIssuer where
  arbitrary = CertificateIssuer
    <$> arbitrary
    <*> pure Nothing -- TODO: test logo as well in the future
    <*> arbitrary

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
    <*> liftArbitrary genArbitraryName
    <*> arbitrary
    <*> arbitrary

instance Arbitrary DAppDTO where
  arbitrary = DAppDTO <$> arbitrary

instance Arbitrary ProfileDTO where
  arbitrary = ProfileDTO
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary UserRole where
  arbitrary = elements [NoRole,Support,Admin]

instance Arbitrary TierType where
-- Developer | Auditor
  arbitrary = elements [Developer,Auditor]

instance Arbitrary WalletAddress where
  arbitrary = do
    (walletAddress :: ProfileWalletAddress) <- arbitrary
    let value = getPatternedText walletAddress
    pure $ WalletAddress value


instance Arbitrary WalletAddressStatus where
  arbitrary = elements [Reserved , Overlapping]

instance Arbitrary DB.ProfileWallet where
  arbitrary = do
    (walletAddress :: ProfileWalletAddress) <- arbitrary
    let value = getPatternedText walletAddress
    DB.ProfileWallet
      <$> arbitrary
      <*> pure value
      <*> arbitrary
      <*> arbitrary

instance Arbitrary PW.ProfileWallet where
  arbitrary = do
    (reservedAddress,status) <- arbitrary
    walletAddress'  <- arbitrary
    balance <- fromInteger <$> choose (0,1000000000000)
    pure $ PW.ProfileWallet
      (reservedAddress,status)
      walletAddress'
      balance

instance Arbitrary SubscriptionLite where
  arbitrary = SubscriptionLite
    <$> arbitrary
    <*> genArbitraryName
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

--ActiveSubscription | InactiveSubscription | PendingSubscription
instance Arbitrary SubscriptionStatus where
  arbitrary = elements [ActiveSubscription, InactiveSubscription, PendingSubscription]

instance Arbitrary RunStats where
  arbitrary = RunStats
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ProfileSummaryDTO where
  arbitrary = do
    profile' <- arbitrary
    stats <- arbitrary
    ProfileSummaryDTO profile'
      <$> arbitrary
      <*> arbitrary
      <*> pure (stats { runsProfileId = profile'.profileId  })
      <*> arbitrary

instance Arbitrary ProfileBody where
  arbitrary = ProfileBody <$> arbitrary <*> arbitrary

-- data CertificationLevel = L0 | L1 | L2 | L3
instance Arbitrary CertificationLevel where
  arbitrary = elements [L0, L1, L2, L3]

instance Arbitrary AuditorReportEvent where
  arbitrary = AuditorReportEvent
    <$> (toId . (+1) . abs <$> arbitrary)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> genArbitraryName

--------------------------------------------------------------------------------
-- | invoice

instance Arbitrary InvoiceId where
  arbitrary = toId . (+1) . abs <$> arbitrary

instance Arbitrary InvoiceBody where
  arbitrary = InvoiceBody
    <$> arbitrary
    <*> arbitrary
    <*> liftArbitrary genArbitraryName
    <*> liftArbitrary genArbitraryName
    <*> arbitrary

instance Arbitrary InvoiceItemBody where
  arbitrary = InvoiceItemBody <$> (InvoiceItem (toId (-1))
    <$> arbitrary
    <*> genArbitraryName
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary)

instance Arbitrary InvoiceItemDTO where
  arbitrary = InvoiceItemDTO <$> (InvoiceItem (toId (-1))
    <$> arbitrary
    <*> genArbitraryName
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary)

instance Arbitrary InvoiceDTO where
  arbitrary = do
    invoice' <- arbitrary
    items' <- listOf arbitrary
    let items'' = flip Prelude.map items' $ \item -> do
        let item' = item { invItemInvId = invId invoice' }
        InvoiceItemDTO item'
    pure $ InvoiceDTO invoice' items''

instance Arbitrary InvoiceItem where
  arbitrary = InvoiceItem
    <$> arbitrary
    <*> arbitrary
    <*> genArbitraryName
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Invoice where
  arbitrary = Invoice
    <$> (toId . (+1) . abs <$> arbitrary)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> genArbitraryName
    <*> genArbitraryName
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
