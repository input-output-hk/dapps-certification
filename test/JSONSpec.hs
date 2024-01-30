{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module JSONSpec (spec) where

import Test.Hspec
import IOHK.Certification.Persistence
import Plutus.Certification.API
import Plutus.Certification.Metadata
import Utils.JSON


import Instances.Persistence ()
import Data.Text (pack)

na :: a
na = undefined

spec :: SpecWith ()
spec = do
  describe "JSON - happy path" $ do
      testJSON (na :: GitHubAccessToken)
      testJSON' "Twitter" (na :: Twitter)
      testJSON' "LinkedIn" (na :: LinkedIn)
      testJSON' "Email" (na :: Email)
      testJSON' "Subject" (na :: Subject)
      testJSON' "GitHubAccount" (na :: GitHubAccount)
      testJSON' "DiscordLink" (na :: DiscordLink)
      testJSON' "CertificationIssuerName" (na :: CertificationIssuerName)
      testJSON' "ProfileWalletAddress" (na :: ProfileWalletAddress)
      testJSON' "Website" (na :: Website)
      testJSON' "Social" (na :: Social)
      testJSON' "CertificateIssuer" (na :: CertificateIssuer)
      testJSON' "Profile" (na :: Profile)
      testJSON' "DApp" (na :: DApp)
      testJSON' "AuditorReportEvent" (na :: AuditorReportEvent)
      testJSONWithEq' eqDAppDTO "DAppDTO" (na :: DAppDTO)
      testJSONWithEq' eqProfileDTO "ProfileDTO" (na :: ProfileDTO)
      testJSONWithEq' eqProfileSummaryDTO "ProfileSummaryDTO" (na :: ProfileSummaryDTO)
      testJSONWithEq' eqProfileBody "ProfileBody" (na :: ProfileBody)

      testJSON' "Invoice" (na :: Invoice)
      testJSON' "InvoiceItem" (na :: InvoiceItem)
      testJSON' "InvoiceDTO" (na :: InvoiceDTO)
      testJSON' "InvoiceItemDTO" (na :: InvoiceItemDTO)
      testJSON' "InvoiceBody" (na :: InvoiceBody)
      testJSON' "InvoiceItemBody" (na :: InvoiceItemBody)

  where

  dummyAddress :: ProfileWalletAddress
  dummyAddress = case mkPatternedText $ "addr1" <> pack (replicate 58 '0') of
    Right a -> a
    Left _ -> error "dummyAddress"
  eqProfileBody :: ProfileBody -> ProfileBody-> Bool
  eqProfileBody (ProfileBody ap ad) (ProfileBody bp bd) =
    let ap' = ap { ownerAddress = dummyAddress }
        bp' = bp { ownerAddress = dummyAddress }
    in ap' == bp' && ad == bd

  eqProfileDTO :: ProfileDTO -> ProfileDTO -> Bool
  eqProfileDTO (ProfileDTO ap ad ar) (ProfileDTO bp bd br) =
    ap == bp && ar == br && case (ad,bd) of
      (Just a, Just b) -> eqDAppDTO a b
      (Nothing, Nothing) -> True
      _ -> False

  eqProfileSummaryDTO :: ProfileSummaryDTO -> ProfileSummaryDTO -> Bool
  eqProfileSummaryDTO (ProfileSummaryDTO ap ar ad aRuns as ) (ProfileSummaryDTO bp br bd bRuns bs ) =
    ap == bp && ar == br && as == bs && aRuns == bRuns && case (ad,bd) of
    (Just a, Just b) -> eqDAppDTO a b
    (Nothing, Nothing) -> True
    _ -> False

  eqDAppDTO :: DAppDTO -> DAppDTO -> Bool
  eqDAppDTO (DAppDTO a) (DAppDTO b)
    | a == b = True
    | otherwise = case (dappGitHubToken a,dappGitHubToken b) of
      (Just _, Nothing) -> a { dappGitHubToken = Nothing } == b
      (Nothing,Just _) -> a == b { dappGitHubToken = Nothing}
      _ -> False
