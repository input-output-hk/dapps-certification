{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module PatternSpec (spec) where

import Test.Hspec
import IOHK.Certification.Persistence
import Plutus.Certification.Metadata
import Data.Aeson
import Data.Text
import Instances.Persistence
import Data.Text.Encoding (encodeUtf8)
import Test.QuickCheck
import Test.Hspec.QuickCheck
import GHC.TypeLits
import Control.Monad (replicateM)
import Data.Data (Proxy(..))

newtype FixedLenAlphaNumerical (len :: Nat) = FixedLenAlphaNumerical Text
  deriving (Show, Eq)

newtype VariableLenAlphaNumerical (min :: Nat) (max :: Nat) = VariableLenAlphaNumerical Text
  deriving (Show, Eq)

newtype InvalidAddressChar = InvalidAddressChar Char
  deriving (Show, Eq)

instance Arbitrary InvalidAddressChar where
  arbitrary = InvalidAddressChar <$> elements "?!@#$%^&*()}{\\|][<>~`}"

newtype LongerAddressLength = LongerAddressLength Int
  deriving (Show, Eq)

instance Arbitrary LongerAddressLength where
  arbitrary = LongerAddressLength <$> choose (99,120)

newtype ShorterAddressLength = ShorterAddressLength Int
  deriving (Show, Eq)

instance Arbitrary ShorterAddressLength where
  arbitrary = ShorterAddressLength <$> choose (0,52)

instance (KnownNat len) => Arbitrary (FixedLenAlphaNumerical len) where
  arbitrary = do
    FixedLenAlphaNumerical . pack <$> replicateM (fromInteger labelN) (elements alphaNum)
    where
    labelN = natVal (Proxy :: Proxy len)

instance (KnownNat min, KnownNat max) => Arbitrary (VariableLenAlphaNumerical min max) where
  arbitrary = do
    len <- fromInteger <$> choose (minN, maxN)
    VariableLenAlphaNumerical . pack <$> replicateM len (elements alphaNum)
    where
    minN :: Integer = natVal (Proxy :: Proxy min)
    maxN :: Integer = natVal (Proxy :: Proxy max)

encodedProfileAddress :: Text -> Either String ProfileWalletAddress
encodedProfileAddress tokenStr = eitherDecodeStrict
  (encodeUtf8 ("\"" <> tokenStr <> "\""))

encodedGithubAccount :: Text -> Either String GitHubAccount
encodedGithubAccount tokenStr = eitherDecodeStrict
  (encodeUtf8 ("\"" <> tokenStr <> "\""))

spec :: SpecWith ()
spec =
  describe "Persistence data validations" $ do
      describe "GitHubAccessToken" $ do

        it "succeeds on decoding valid token" $ do
          let token = "\"ghp_000000000000000000000000000000000000\""
          decode token `shouldBe` Just (GitHubAccessToken PersonalToken "000000000000000000000000000000000000")

        it "fails on decoding invalid token because of prefix" $ do
          let token = "\"ghX_000000000000000000000000000000000000\""
          eitherDecode token
            `shouldBe`
            (Left "Error in $: invalid access token type" :: Either String GitHubAccessToken)

        it "fails on decoding invalid token because of suffix" $ do
          let token = "\"ghp_!@#@#@%#@$%@#%@#$%@\""
          eitherDecode token
            `shouldBe`
            (Left "Error in $: invalid suffix" :: Either String GitHubAccessToken)

        it "fails on decoding invalid token" $ do
          let token = "\"asdadas\""
          eitherDecode token
            `shouldBe`
            (Left "Error in $: invalid token" :: Either String GitHubAccessToken)

      describe "ProfileWalletAddress JSON encode/decode" $ do

        let token = "qrh2dmheg8mnxjzrw0muya97x2adhyyesqmqahhjswkjh8yg672ldf7xjwvpv77022vxtjy33g7luypk2wygaqst3r7qxu0fd2"
        let stakeToken = "urdyxqs76a3zcwjpkym0kc2xsq26elcartf5gcmp0gn58ms32qx04"

        it "fails on \"\"" $
          decode "" `shouldBe` (Nothing :: Maybe ProfileWalletAddress)

        it "decodes correctly for change address on mainnet" $ do
          let fullTokenStr =  "addr1" <> token
          (show <$> encodedProfileAddress fullTokenStr)
            `shouldBe` Right (show fullTokenStr)

        it "decodes correctly for change address on testnet" $ do
          let fullTokenStr =  "addr_test1" <> token
          (show <$> encodedProfileAddress fullTokenStr)
            `shouldBe` Right (show fullTokenStr)

        it "decodes correctly for stake address on mainnet" $ do
          let fullTokenStr =  "stake" <> stakeToken
          (show <$> encodedProfileAddress fullTokenStr)
            `shouldBe` Right (show fullTokenStr)

        it "decodes correctly for stake address on testnet" $ do
          let fullTokenStr =  "stake_test1" <> stakeToken
          (show <$> encodedProfileAddress fullTokenStr)
            `shouldBe` Right (show fullTokenStr)

        prop "fails on short address" $ \(ShorterAddressLength n) -> do
          let _shortText = pack $ Prelude.replicate n '0'
          decodeStrict (encodeUtf8 $ "\"addr1" <> _shortText  <> "\"")
          `shouldBe`
          (Nothing :: Maybe ProfileWalletAddress)

        prop "fails on invalid characters" $ \(InvalidAddressChar c) -> do
          let _99LongText = pack $ Prelude.replicate 98 c
          decodeStrict (encodeUtf8 $ "\"addr1" <> _99LongText <> "\"")
          `shouldBe`
          (Nothing :: Maybe ProfileWalletAddress)

      describe "GitHubAccount serialization" $ do

        prop "succeeds on valid strings ranging between a length of 1-39" $ \(
          VariableLenAlphaNumerical @1 @39 n1) -> do
          (show <$> encodedGithubAccount n1)
            `shouldBe` Right (show n1)

        prop "succeeds on valid strings with multiple hyphens" $ \
          ( VariableLenAlphaNumerical @1 @5 n1
          , VariableLenAlphaNumerical @1 @5 n2
          , VariableLenAlphaNumerical @1 @5 n3) -> do

          let _text = n1 <> "-" <> n2 <> "-" <> n3
          (show <$> encodedGithubAccount _text)
            `shouldBe` Right (show _text)

        prop "fails on valid string longer than 39" $ \(
          VariableLenAlphaNumerical @40 @100 n1) -> do
          (show <$> encodedGithubAccount n1) `shouldBe`
            Left "Error in $: Invalid value for pattern ^(?=.{1,39}$)[a-zA-Z0-9]+(-[a-zA-Z0-9]+)*$"

        it "fails on empty string" do
          (show <$> encodedGithubAccount "") `shouldBe`
            Left "Error in $: Invalid value for pattern ^(?=.{1,39}$)[a-zA-Z0-9]+(-[a-zA-Z0-9]+)*$"

        prop "fails because of double hyphen" $ \(
          FixedLenAlphaNumerical @30 n1) -> do
          let _text = "\"" <> n1 <> "--gh\""
          decodeStrict (encodeUtf8 _text)
          `shouldBe`
          (Nothing :: Maybe GitHubAccount)

        prop "fails because starts with hyphen" $ \(
          VariableLenAlphaNumerical @1 @38 t ) -> do
          let _text = "\"-" <> t <> "\""
          decodeStrict (encodeUtf8 _text)
          `shouldBe`
          (Nothing :: Maybe GitHubAccount)

        prop "fails because it is ending with hyphen" $ \(
          VariableLenAlphaNumerical @1 @38 t ) -> do
          let _text = "\"" <> t <> "-\""
          decodeStrict (encodeUtf8 _text)
          `shouldBe`
          (Nothing :: Maybe GitHubAccount)

        prop "fails because of invalid chars" $ \(
          VariableLenAlphaNumerical @1 @10 t,InvalidAddressChar text ) -> do
          let _text = "\"" <> t <> pack [text] <> "\""
          decodeStrict (encodeUtf8 _text)
          `shouldBe`
          (Nothing :: Maybe GitHubAccount)
