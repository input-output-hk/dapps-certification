{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE TupleSections         #-}
module ProfileWallet.TxProcessingSpec (processingTxSpec) where

import Test.QuickCheck
--import IOHK.Certification.Persistence
import Data.Text()
import Test.QuickCheck.Instances.Time ()
import Data.List (find, sortBy)
import Test.Hspec.QuickCheck

import Test.Hspec
import Data.Function (on)
import Plutus.Certification.ProfileWallet as PW
import Data.Word (Word64)
import Instances.Persistence ()

--------------------------------------------------------------------------------
-- | UTILITIES

data WalletsWithSimplePayment = WalletsWithSimplePayment
    [PW.ProfileWallet] WalletAddress Word64
                              deriving (Eq, Show)

instance Arbitrary WalletsWithSimplePayment where
  arbitrary = do
    wallets <- arbitrary
    (paidIntoAddress,amount) <- simplePaymentArgs wallets
    pure $ WalletsWithSimplePayment wallets paidIntoAddress amount


data WalletsWithDesignatedPayment = WalletsWithDesignatedPayment
    [PW.ProfileWallet] ProfileWalletAddress WalletAddress Word64
                              deriving (Eq, Show)
instance Arbitrary WalletsWithDesignatedPayment where
  arbitrary = do
    wallets <- arbitrary
    (profileAddress,paidIntoAddress,amount) <- designatedPaymentArgs wallets
    pure $ WalletsWithDesignatedPayment wallets
      profileAddress paidIntoAddress amount


data WalletsWithWalletAddressAssignment = WalletsWithWalletAddressAssignment
    [PW.ProfileWallet] ProfileWalletAddress WalletAddress
                              deriving (Eq, Show)
instance Arbitrary WalletsWithWalletAddressAssignment where
  arbitrary = do
    wallets <- arbitrary
    (profileAddress, addressAssignment') <- walletAddressAssignmentArgs wallets
    pure $ WalletsWithWalletAddressAssignment wallets profileAddress
      addressAssignment'

data WalletsWithPayment = WalletsWithPayment
    [PW.ProfileWallet] PW.Transaction
                              deriving (Eq, Show)


instance Arbitrary WalletsWithPayment where
  arbitrary = do
    wallets <- arbitrary
    WalletsWithPayment wallets <$> oneof
      [ simplePayment wallets
      , designatedPayment wallets
      , addressAssignment wallets
      ]

-- | Generates a simple payment transaction
-- 1. we choose a wallet from the list
-- 2. we choose a paidIntoAddress
--  2.1 we choose a paidIntoAddress from the list or we generate a new one
simplePaymentArgs :: [PW.ProfileWallet] -> Gen (WalletAddress,Word64)
simplePaymentArgs wallets = do
  let walletAddresses' :: [WalletAddress] = fmap (snd . pwAddressReservation) wallets
  getItFromWalletList :: Bool <- arbitrary
  paidIntoAddress :: WalletAddress <- if getItFromWalletList && not (null walletAddresses')
    then elements walletAddresses'
    else arbitrary
  (paidIntoAddress,) <$> arbitrary

-- | Generates a simple payment transaction
simplePayment :: [PW.ProfileWallet] -> Gen PW.Transaction
simplePayment wallets = do
  (paidIntoAddress,amount) <- simplePaymentArgs wallets
  pure $ SimplePayment paidIntoAddress amount

-- | Generates a designated payment transaction
designatedPayment :: [PW.ProfileWallet] -> Gen PW.Transaction
designatedPayment wallets = do
  (profileAddress,paidIntoAddress,amount) <- designatedPaymentArgs wallets
  pure $ DesignatedPayment profileAddress paidIntoAddress amount

-- how it works:
-- 1. we choose a wallet from the list
-- 2. we choose a profile address
--  2.1. we choose a profile address from the list or we generate a new one
-- 3. we choose a paidIntoAddress
--  3.1 we choose a paidIntoAddress from the list or we generate a new one
designatedPaymentArgs :: [PW.ProfileWallet] -> Gen (ProfileWalletAddress, WalletAddress, Word64)
designatedPaymentArgs wallets = do
  getPaidIntoFromWalletList :: Bool <- arbitrary
  getProfileFromAlreadyProfileWallets :: Bool <- arbitrary

  -- we choose a wallet
  profileWallet <- if null wallets then arbitrary else elements wallets

  -- this has to be from wallet
  profileAddress :: ProfileWalletAddress <- if getProfileFromAlreadyProfileWallets
    then pure $ profileWallet.pwProfileAddress
    else arbitrary

  paidIntoAddress :: WalletAddress <- if getPaidIntoFromWalletList
        then pure $ snd profileWallet.pwAddressReservation
        else arbitrary
  (profileAddress, paidIntoAddress,) <$> arbitrary

walletAddressAssignmentArgs :: [PW.ProfileWallet] -> Gen (ProfileWalletAddress, WalletAddress)
walletAddressAssignmentArgs wallets = do
  getProfileFromAlreadyProfileWallets :: Bool <- arbitrary

  -- we choose a wallet
  profileWallet <- if null wallets then arbitrary else elements wallets

  -- this has to be from wallet
  profileAddress :: ProfileWalletAddress <- if getProfileFromAlreadyProfileWallets
    then pure $ profileWallet.pwProfileAddress
    else arbitrary
  (profileAddress,) <$> arbitrary

-- | Generates a wallet address assignment transaction
addressAssignment :: [PW.ProfileWallet] -> Gen PW.Transaction
addressAssignment wallets = do
  (profileAddress, addressAssignment') <- walletAddressAssignmentArgs wallets
  pure $ WalletAddressAssignment profileAddress addressAssignment'

byStatus :: WalletAddressStatus
         -> WalletAddressStatus
         -> Ordering
byStatus Reserved Reserved = EQ
byStatus Reserved _        = LT
byStatus _ Reserved        = GT
byStatus _ _               = EQ

--------------------------------------------------------------------------------
-- | ACTUAL TESTS

processingTxSpec :: SpecWith ()
processingTxSpec =
  describe "payments processing" $ do

    -- ADDRESS ASSIGNMENT
    -- ------------------

    describe "address assignation" $ do

      prop "wallet address assignment should remain with no extraMoney" $
        \ (WalletsWithWalletAddressAssignment wallets profileAddress addressAssignment') -> do
        let (_,extraMoney) = updateProfileWallets wallets
              (WalletAddressAssignment profileAddress addressAssignment')
        extraMoney `shouldBe` 0

      prop "wallet address assignment should change the address reservation" $
        \ (WalletsWithWalletAddressAssignment wallets profileAddress addressAssignment') -> do
        let (newWallets,_) = updateProfileWallets wallets
              (WalletAddressAssignment profileAddress addressAssignment')
        let walletM = find (\w -> pwProfileAddress w == profileAddress) newWallets
        -- NOTE: if the generated addressAssignment' is already reserved
        -- the test will fail because nothing will change
        -- though the addressAssignment' collision is improbable
        case walletM of
          Just PW.ProfileWallet{..} -> pwAddressReservation `shouldBe` (Reserved,addressAssignment')
          _ -> expectationFailure "wallet not found"

      prop "wallet address assignment shouldn't change anything if there is a conflict on a reservation" $
        \ (WalletsWithWalletAddressAssignment wallets profileAddress _) -> do
          -- find any reserved address Reserved
          let targetedReservation = find (\w -> fst w.pwAddressReservation == Reserved
                              && w.pwProfileAddress /= profileAddress) wallets

          case targetedReservation of
            Just wallet -> do
              let (newWallets,_) = updateProfileWallets wallets
                    (WalletAddressAssignment profileAddress (snd $ pwAddressReservation wallet))
              newWallets `shouldBe` wallets
            _ -> return ()

    -- SIMPLE PAYMENTS
    -- ---------------
    describe "simple payments" $ do

      -- expected behavior:
        -- if a simple payment was made to a wallet that has that particular address reserved
        -- then the extraMoney should be 0
        -- if a simple payment was made to a wallet that has a different address reserved or the address is overlapping
        -- then the extraMoney should be the amount paid
      prop "a simple payment should remain with proper extraMoney" $
        \ (WalletsWithSimplePayment wallets paidIntoAddress amount) -> do
        -- if wallet already exists
        let walletM = find (\w -> snd w.pwAddressReservation == paidIntoAddress) wallets
        let (_,extraMoney) = updateProfileWallets wallets (SimplePayment paidIntoAddress amount)
        case walletM of
          Just PW.ProfileWallet{..} | fst pwAddressReservation == Reserved
            -> extraMoney `shouldBe` 0
          _ -> extraMoney `shouldBe` amount

      prop "a simple payment should increase the total amount with the sum paid" $
        \ (WalletsWithSimplePayment wallets paidIntoAddress amount) -> do

        -- if wallet already exists
        let (newWallets,extraMoney) = updateProfileWallets wallets (SimplePayment paidIntoAddress amount)

        -- new resulting wallets + extraMoney should be equal to the old wallets + amount
        sum (fmap pwBalance newWallets) + extraMoney
          `shouldBe`
          sum (fmap pwBalance wallets) + amount

      prop "a simple payment should never add any wallet" $
        \ (WalletsWithSimplePayment wallets paidIntoAddress amount) -> do

        -- if wallet already exists
        let (newWallets,_) = updateProfileWallets wallets (SimplePayment paidIntoAddress amount)
        length newWallets `shouldBe` length wallets

    -- DESIGNATED PAYMENTS
    -- -------------------
    describe "designated payments" $ do

      prop "a designated payment should alway transfer money to the designated wallet" $
        \ (WalletsWithDesignatedPayment wallets profileAddress paidIntoAddress amount) -> do

          let (newWallets,_) = updateProfileWallets wallets
                (DesignatedPayment profileAddress paidIntoAddress amount)
              profileHasAddress = (== profileAddress) . pwProfileAddress
              sumProfileAmount = sum . fmap pwBalance . filter profileHasAddress

          sumProfileAmount newWallets - sumProfileAmount wallets `shouldBe` amount

      prop "a designated payment should always remain with 0 extraMoney" $
        \ (WalletsWithDesignatedPayment wallets profileAddress paidIntoAddress amount) -> do

        let (_,extraMoney) = updateProfileWallets wallets (DesignatedPayment profileAddress paidIntoAddress amount)
        extraMoney `shouldBe` 0

      prop "a designated payment should not make any modifications on other profiles wallets" $
        \ (WalletsWithDesignatedPayment wallets profileAddress paidIntoAppWalletAddress amount) -> do

        let profileHasAddress = (== profileAddress) . pwProfileAddress

            -- update the wallets
            (newWallets,_) = updateProfileWallets wallets
                (DesignatedPayment profileAddress paidIntoAppWalletAddress amount)

        -- in all cases the other wallets should remain unchanged
        filter (not . profileHasAddress) newWallets
          `shouldBe`
           filter (not . profileHasAddress) wallets

      prop "a designated payment should make the right address reservation" $
        \ (WalletsWithDesignatedPayment wallets profileAddress paidIntoAppWalletAddress amount) -> do

         -- perquisites
        let profileUsingAppWalletAddress =
              -- we sort the wallets by the status of the address reservation
              -- because we want to find the first wallet that
              -- has the address reserved
              find addressUsed
                (sortBy (byStatus `on` ( fst . pwAddressReservation )) wallets)
            profileHasAddress = (== profileAddress) . pwProfileAddress
            addressUsed = (== paidIntoAppWalletAddress) . snd . pwAddressReservation

            -- update the wallets
            (newWallets,_) = updateProfileWallets wallets
                (DesignatedPayment profileAddress paidIntoAppWalletAddress amount)

        -- let's see what happened
        case profileUsingAppWalletAddress of

          -- if there was a designated Reserved address for the wallet,
          -- the balance of that profile should be increased with the amount
          -- and the new application-wallet-address should not be used
          Nothing
            | (Just oldWallet@(ProfileWallet (Reserved, _) _ oldAmount))
                <- find profileHasAddress wallets  -> do

              newWallets
                `shouldContain`
                [oldWallet { pwBalance = oldAmount + amount }]
              length newWallets `shouldBe` length wallets

          -- if there was a prior designated address for the wallet
          -- but wasn't Reserved
          -- the balance of the wallet should be increased with the `amount`
          -- and the new application-wallet-address should be used instead
          Nothing
            | (Just (ProfileWallet (Overlapping, _) _ oldAmount))
                <- find profileHasAddress wallets  -> do
              newWallets
                `shouldContain`
                [PW.ProfileWallet
                  (Reserved,paidIntoAppWalletAddress)
                  profileAddress (oldAmount + amount)
                ]
              length newWallets `shouldBe` length wallets

          -- if there was no prior activity for this profile
          -- there has to be a new wallet with the designated address
          Nothing | not $ any profileHasAddress wallets  -> do
            newWallets
              `shouldContain`
              [PW.ProfileWallet
                (Reserved,paidIntoAppWalletAddress)
                profileAddress amount
              ]
            length newWallets `shouldBe` length wallets + 1

          -- 1. there is already a reserved address for the app-wallet-address
          -- 2. that profiles is different from the tested profile
          -- 3. there is no prior activity for the tested profile
          -- expected behavior:
          --  - a new wallet should be created with the Overlapping address
          Just (ProfileWallet (Reserved,_) theOtherProfileAddress _)
            | theOtherProfileAddress  /= profileAddress
            , Nothing <- find profileHasAddress wallets  -> do
              newWallets
                `shouldContain`
                [PW.ProfileWallet
                  (Overlapping,paidIntoAppWalletAddress)
                  profileAddress amount
                ]
              init newWallets `shouldBe` wallets

          -- 1. there is already a reserved address for the app-wallet-address
          -- 2. that profiles is different from the tested profile
          -- 3. there is prior activity for the tested profile
          -- expected behavior:
          --  - the old wallet should be modified with the accumulated amount
          --  - the old wallet will preserve its address reservation (Overlapping or Reserved)
          --  - no new wallet should be created
          Just (ProfileWallet (Reserved,_) theOtherProfileAddress _)
            | theOtherProfileAddress  /= profileAddress
            , (Just (ProfileWallet (status,oldAppAddress) _ oldAmount ))
                <- find profileHasAddress wallets  -> do

              newWallets
                `shouldContain`
                [PW.ProfileWallet (status,oldAppAddress) profileAddress (oldAmount + amount)]
              length newWallets `shouldBe` length wallets

          ---- from this point on no other wallet should have the app-wallet-address
          ---- reserved

          -- case 1. there is no prior activity for the tested profile
          -- case 2. there is prior activity with an overlapping address
          -- case 3. there is prior activity with a reserved address


          -- case 1. there is no prior activity for the tested profile
          -- expected behavior:
          -- - a new wallet should be created with the Reserved address
          -- - new wallet should be added to the list
          Just _
            | not $ any profileHasAddress wallets  -> do

              newWallets
                `shouldContain`
                [PW.ProfileWallet
                  (Reserved,paidIntoAppWalletAddress)
                  profileAddress amount
                ]
              init newWallets `shouldBe` wallets

          -- case 2. there is prior activity with an overlapping address
          -- expected behavior:
          -- - the old wallet should be modified with the accumulated amount
          -- - the old wallet will get the new app-wallet-address reservation
          -- - no new wallet should be added
            | (Just (ProfileWallet (Overlapping,_) _ oldAmount ))
                <- find profileHasAddress wallets  -> do

              newWallets
                `shouldContain`
                [PW.ProfileWallet
                  (Reserved,paidIntoAppWalletAddress)
                  profileAddress (oldAmount + amount)
                ]
              length newWallets `shouldBe` length wallets

          -- case 3. there is prior activity with a reserved address
          -- expected behavior:
          -- - the old wallet should be modified with the accumulated amount
          -- - the old wallet will preserve its address reservation
          -- - no new wallet should be added
            | (Just (ProfileWallet (Reserved,oldAppAddress) _ oldAmount))
                <- find profileHasAddress wallets  -> do

              newWallets
                `shouldContain`
                [PW.ProfileWallet (Reserved,oldAppAddress) profileAddress (oldAmount + amount)]
              length newWallets `shouldBe` length wallets

          _ -> expectationFailure "all cases should be covered"
