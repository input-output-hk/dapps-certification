{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}
module PatternSpec (spec) where

import Test.Hspec
import IOHK.Certification.Persistence
import Data.Aeson
import Data.Text
import Instances.Persistence ()
import Data.Text.Encoding (encodeUtf8)
import Test.QuickCheck
import Test.Hspec.QuickCheck

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

encodedProfileAddress tokenStr = eitherDecodeStrict
  @ProfileWalletAddress (encodeUtf8 ("\"" <> tokenStr <> "\""))
spec :: SpecWith ()
spec =
  describe "Persistence data validations" $ do

      describe "GitHubAccessToken" $ do

        it "should succeed on decoding valid token" $ do
          let token = "\"ghp_000000000000000000000000000000000000\""
          decode token `shouldBe` Just (GitHubAccessToken PersonalToken "000000000000000000000000000000000000")

        it "should fail on decoding invalid token because of prefix" $ do
          let token = "\"ghX_000000000000000000000000000000000000\""
          eitherDecode token
            `shouldBe`
            (Left "Error in $: invalid access token type" :: Either String GitHubAccessToken)

        it "should fail on decoding invalid token because of suffix" $ do
          let token = "\"ghp_!@#@#@%#@$%@#%@#$%@\""
          eitherDecode token
            `shouldBe`
            (Left "Error in $: invalid suffix" :: Either String GitHubAccessToken)

        it "should fail on decoding invalid token" $ do
          let token = "\"asdadas\""
          eitherDecode token
            `shouldBe`
            (Left "Error in $: invalid token" :: Either String GitHubAccessToken)

      describe "ProfileWalletAddress JSON encode/decode" $ do

        let token = "qrh2dmheg8mnxjzrw0muya97x2adhyyesqmqahhjswkjh8yg672ldf7xjwvpv77022vxtjy33g7luypk2wygaqst3r7qxu0fd2"
        let stakeToken = "urdyxqs76a3zcwjpkym0kc2xsq26elcartf5gcmp0gn58ms32qx04"

        it "should fail on \"\"" $
          decode "" `shouldBe` (Nothing :: Maybe ProfileWalletAddress)

        it "should decode correctly for change address on mainnet" $ do
          let fullTokenStr =  "addr1" <> token
          (show <$> encodedProfileAddress fullTokenStr)
            `shouldBe` Right (show fullTokenStr)

        it "should decode correctly for change address on testnet" $ do
          let fullTokenStr =  "addr_test1" <> token
          (show <$> encodedProfileAddress fullTokenStr)
            `shouldBe` Right (show fullTokenStr)

        it "should decode correctly for stake address on mainnet" $ do
          let fullTokenStr =  "stake" <> stakeToken
          (show <$> encodedProfileAddress fullTokenStr)
            `shouldBe` Right (show fullTokenStr)

        it "should decode correctly for stake address on testnet" $ do
          let fullTokenStr =  "stake_test1" <> stakeToken
          (show <$> encodedProfileAddress fullTokenStr)
            `shouldBe` Right (show fullTokenStr)

        prop "should fail on short address" $ \(ShorterAddressLength n) -> do
          let _shortText = pack $ Prelude.replicate n '0'
          decodeStrict (encodeUtf8 $ "\"addr1" <> _shortText  <> "\"")
          `shouldBe`
          (Nothing :: Maybe ProfileWalletAddress)

        prop "should fail on invalid characters" $ \(InvalidAddressChar c) -> do
          let _99LongText = pack $ Prelude.replicate 98 c
          decodeStrict (encodeUtf8 $ "\"addr1" <> _99LongText <> "\"")
          `shouldBe`
          (Nothing :: Maybe ProfileWalletAddress)
