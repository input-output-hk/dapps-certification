{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module ProfileWalletSpec (spec) where

import Test.Hspec
import ProfileWallet.TxPreProcessingSpec
import ProfileWallet.TxProcessingSpec

spec :: SpecWith ()
spec =
  describe "Profile wallet" $ do
    fromDbTransactionSpec
    processingTxSpec
