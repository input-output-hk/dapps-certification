{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module ProfileWallet.TxPreProcessingSpec (fromDbTransactionSpec) where

import Test.Hspec
import Plutus.Certification.ProfileWallet
import ProfileWallet.Data
import Data.Text
import Text.RawString.QQ(r)
import Data.Int
import IOHK.Certification.Persistence

isOurAddress :: Text -> Bool
isOurAddress = flip Prelude.elem walletAddresses

fromDbTransactionSpec :: SpecWith ()
fromDbTransactionSpec =
  describe "payment pre-processing" $ do
    it "successfully reserves an address" $ do
      let amount :: Int64 = 9
      let tx = MinimalTransaction
            { mtxTxId = toId 0
            , mtxAmount = amount
            , mtxMetadata = [r| {
              "0": {
                "map": [ {
                    "k": { "string": "payer" },
                    "v": {
                      "list": [
                        { "string": "addr_test1qqy3xyv0n4s7fm2ww7yutgzuhvvsf9mvyffmtndmmkg53er3ydg8gs" },
                        { "string": "frl2eh3h26v74qvhyqvj992v7qyvqqrk2pf4kqaaahsu" }
                      ] } } ] } } |]
            }
      let payer = "addr_test1qqy3xyv0n4s7fm2ww7yutgzuhvvsf9mvyffmtndmmkg53er3ydg8gsfrl2eh3h26v74qvhyqvj992v7qyvqqrk2pf4kqaaahsu"
      let walletAddress = "addr_test1qzj3v7pqlltaxk38c4nyv73gutm2ldl4e6atwewhwvt6yljt5w2gu33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asaf5p6s"
      let entries = [
              MinimalTransactionEntry
                (toId 0)
                payer
                True
            , MinimalTransactionEntry
                (toId 0)
                payer
                False
            , MinimalTransactionEntry
                (toId 0)
                walletAddress
                False
            ]
      let resp = fromDbTransaction isOurAddress 0 tx entries
      payer' <- case mkPatternedText payer :: Either String ProfileWalletAddress of
            Right x -> pure x
            Left _ -> fail "Failed to create ProfileWalletAddress"
      resp `shouldBe`
        Right (DesignatedPayment
                payer'
                (WalletAddress walletAddress)
                (fromIntegral amount))

    it "handles address assignment with an internal error (no wallet address)" $ do
      let amount :: Int64 = -5
      let tx = MinimalTransaction
            { mtxTxId = toId 0
            , mtxAmount = amount
            , mtxMetadata = [r| {
              "0": {
                "map": [ {
                    "k": { "string": "assignment" },
                    "v": {
                      "list": [
                        { "string": "addr_test1qqy3xyv0n4s7fm2ww7yutgzuhvvsf9mvyffmtndmmkg53er3ydg8gs" },
                        { "string": "frl2eh3h26v74qvhyqvj992v7qyvqqrk2pf4kqaaahsu" }
                      ] } } ] } } |]
            }
      let address = "addr_test1qqy3xyv0n4s7fm2ww7yutgzuhvvsf9mvyffmtndmmkg53er3ydg8gs"
      let entries = [
              MinimalTransactionEntry
                (toId 1)
                address
                False
              -- here is the problem, the address is not in the walletAddresses list
            ]
      let resp = fromDbTransaction isOurAddress 0 tx entries
      resp `shouldBe` Left "InternalError: no wallet address found for address assignment"

    it "handles address assignment with our wallet address" $ do
      let amount :: Int64 = -5
      let tx = MinimalTransaction
            { mtxTxId = toId 0
            , mtxAmount = amount
            , mtxMetadata = [r| {
              "0": {
                "map": [ {
                    "k": { "string": "assignment" },
                    "v": {
                      "list": [
                        { "string": "addr_test1qzj3v7pqlltaxk38c4nyv73gutm2ldl4e6atwewhwvt6yljt5w2g" },
                        { "string": "u33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asaf5p6s" }
                      ] } } ] } } |]
            }
      let walletAddress = "addr_test1qzj3v7pqlltaxk38c4nyv73gutm2ldl4e6atwewhwvt6yljt5w2gu33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asaf5p6s"
      let entries = [
            MinimalTransactionEntry
                (toId 2)
                walletAddress
                False
            ]
      let resp = fromDbTransaction isOurAddress 0 tx entries
      resp `shouldBe` Left "Address assignment shouldn't belong to our wallet"

    it "handles address assignment with withdrawal" $ do
      let amount :: Int64 = 5
      let tx = MinimalTransaction
            { mtxTxId = toId 0
            , mtxAmount = amount
            , mtxMetadata = [r| {
              "0": {
                "map": [ {
                    "k": { "string": "assignment" },
                    "v": {
                      "list": [
                        { "string": "addr_test1qqy3xyv0n4s7fm2ww7yutgzuhvvsf9mvyffmtndmmkg53er3ydg8gs" },
                        { "string": "frl2eh3h26v74qvhyqvj992v7qyvqqrk2pf4kqaaahsu" }
                      ] } } ] } } |]
            }
      let walletAddress = "addr_test1qzj3v7pqlltaxk38c4nyv73gutm2ldl4e6atwewhwvt6yljt5w2gu33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asaf5p6s"
      let entries = [
              MinimalTransactionEntry
                (toId 3)
                walletAddress
                False
            ]
      let resp = fromDbTransaction isOurAddress 0 tx entries
      resp `shouldBe` Left "Address assignment transaction must be a withdrawal"

    it "handles payer address which belongs to our wallet" $ do
      let amount :: Int64 = 8
      let tx = MinimalTransaction
            { mtxTxId = toId 5
            , mtxAmount = amount
            , mtxMetadata = [r| {
              "0": {
                "map": [ {
                    "k": { "string": "payer" },
                    "v": {
                      "list": [
                        { "string": "addr_test1qzj3v7pqlltaxk38c4nyv73gutm2ldl4e6atwewhwvt6yljt5w2g" },
                        { "string": "u33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asaf5p6s" }
                      ] } } ] } } |]
            }
      let walletAddress = "addr_test1qzj3v7pqlltaxk38c4nyv73gutm2ldl4e6atwewhwvt6yljt5w2gu33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asaf5p6s"
      let entries = [
            MinimalTransactionEntry
                (toId 5)
                walletAddress
                False
            ]
      let resp = fromDbTransaction isOurAddress 0 tx entries
      resp `shouldBe` Left "Payer address should not belong to our wallet"

    it "fails if payment is less than minimal" $ do
      let amount :: Int64 = 9
      let tx = MinimalTransaction
            { mtxTxId = toId 0
            , mtxAmount = amount
            , mtxMetadata = [r| {
              "0": {
                "map": [ {
                    "k": { "string": "payer" },
                    "v": {
                      "list": [
                        { "string": "addr_test1qqy3xyv0n4s7fm2ww7yutgzuhvvsf9mvyffmtndmmkg53er3ydg8gs" },
                        { "string": "frl2eh3h26v74qvhyqvj992v7qyvqqrk2pf4kqaaahsu" }
                      ] } } ] } } |]
            }
      let walletAddress = "addr_test1qzj3v7pqlltaxk38c4nyv73gutm2ldl4e6atwewhwvt6yljt5w2gu33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asaf5p6s"
      let entries = [ MinimalTransactionEntry
                (toId 0)
                walletAddress
                False
            ]
      let resp = fromDbTransaction isOurAddress 10 tx entries
      resp `shouldBe` Left "Transaction amount is less than minimum amount"

    it "simple payment due to no metadata" $ do
      let amount :: Int64 = 9
      let tx = MinimalTransaction
            { mtxTxId = toId 0
            , mtxAmount = amount
            , mtxMetadata = [r| { "0": { } } |]
            }
      let walletAddress = "addr_test1qzj3v7pqlltaxk38c4nyv73gutm2ldl4e6atwewhwvt6yljt5w2gu33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asaf5p6s"
      let entries = [ MinimalTransactionEntry
                (toId 0)
                walletAddress
                False
            ]
      let resp = fromDbTransaction isOurAddress 10 tx entries
      resp `shouldBe`  Right (SimplePayment (WalletAddress walletAddress) (fromIntegral amount))

    it "not parsable transaction fails with proper error" $ do
      let amount :: Int64 = -9
      let tx = MinimalTransaction
            { mtxTxId = toId 0
            , mtxAmount = amount
            , mtxMetadata = [r| { "0": { } } |]
            }
      let entries = []
      let resp = fromDbTransaction isOurAddress 10 tx entries
      resp `shouldBe` Left "Transaction is not a standard payment"

