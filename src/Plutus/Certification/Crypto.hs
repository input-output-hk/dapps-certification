
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Plutus.Certification.Crypto where

import Crypto.PubKey.Ed25519 (PublicKey, Signature, toPublic, verify,signature)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base16 (decode)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import "cryptonite" Crypto.Error
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.Serialise as Serialise

import qualified Data.ByteString.Lazy as LBS
import Crypto.WebAuthn.Cose.PublicKeyWithSignAlg
import Crypto.WebAuthn.Cose.PublicKey


{-# LANGUAGE OverloadedStrings #-}
signatureBS2 = BS.pack "845846a201276761646472657373583900f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc0b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe8281a166686173686564f4581c736473616461736468656c6c6f20776f726c642f2f2f617364736164584037dbb6427cfb6587bfa5ef6d999c80aacd37d3739dbfbbd597d7da1231fc06aec3aa918e48e2c10a5cabeddb588cafaba1d90ffa98206f36ca2f3dd417de050a"
signature2 = signature signatureBS2

toSig sig= case decode sig of
    Left _ -> error "invalid signature"
    Right bs -> case signature bs of
      CryptoFailed e -> error $ show e
      CryptoPassed s -> s

-- >>> fromRight $ decode (BS.pack "845846a201276761646472657373583900f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc0b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe8281a166686173686564f4581c736473616461736468656c6c6f20776f726c642f2f2f617364736164584037dbb6427cfb6587bfa5ef6d999c80aacd37d3739dbfbbd597d7da1231fc06aec3aa918e48e2c10a5cabeddb588cafaba1d90ffa98206f36ca2f3dd417de050a")
-- Right "\132XF\162\SOH'gaddressX9\NUL\243u\153h\227\148<\185\142T\NAK\223\164\224\208I\138*a\241\US\ENQ\RS\150\&1\140\204\252\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129\161fhashed\244X\FSsdsadasdhello world///asdsadX@7\219\182B|\251e\135\191\165\239m\153\156\128\170\205\&7\211s\157\191\187\213\151\215\218\DC21\252\ACK\174\195\170\145\142H\226\193\n\\\171\237\219X\140\175\171\161\217\SI\250\152 o6\202/=\212\ETB\222\ENQ\n"

-- >>> toSig signatureBS2
-- invalid signature

-- >>> fromRight $ decode "a4010103272006215820dda5ce7572d07a0b86c9b9951b081b9b67ddbd16c4e1a7df7607c7b069557009"
-- "\164\SOH\SOH\ETX' \ACK!X \221\165\206ur\208z\v\134\201\185\149\ESC\b\ESC\155g\221\189\SYN\196\225\167\223v\a\199\176iUp\t"

-- NOTE: THIS IS THE KEY !!!!
-- >>> decodeCosePublicKey keyOne
-- Right (PublicKeyWithSignAlgInternal {publicKeyInternal = PublicKeyEdDSA {eddsaCurve = CoseCurveEd25519, eddsaX = "\221\165\206ur\208z\v\134\201\185\149\ESC\b\ESC\155g\221\189\SYN\196\225\167\223v\a\199\176iUp\t"}, signAlgInternal = CoseSignAlgEdDSA})
-- >>> eddsaX $ decodeCosePublicKey keyOne
-- Variable not in scope:
--   eddsaX :: Either String PublicKeyWithSignAlg -> b_aFWj[sk:1]

--ss PublicKeyWithSignAlg
    --{ publicKey = PublicKey Cose.PublicKeyEdDSA {eddsaCurve = Cose.CoseCurveEd25519, ..},
      --signAlg = Cose.CoseSignAlgEdDSA
    --} = undefined

fromRight (Left err) =  error $ show err
fromRight (Right x) = x

keyOne :: LBS.ByteString
keyOne =
  let bs = fromRight $ decode "a4010103272006215820dda5ce7572d07a0b86c9b9951b081b9b67ddbd16c4e1a7df7607c7b069557009"
  in LBS.fromStrict bs

verifyDataSignature :: ByteString -> ByteString -> Maybe ByteString -> Maybe ByteString -> Bool
{-verifyDataSignature signatureBS keyBS (Just messageBS) (Just addressBS) = verify sig pubKey (mconcat [messageBS, addressBS])-}
  {-where-}
    {-sig = fromJust $ decode signatureBS :: Signature-}
    {-pubKey = toPublic $ fromJust $ decode keyBS :: PublicKey-}
{-verifyDataSignature signatureBS keyBS (Just messageBS) Nothing = verify sig pubKey messageBS-}
  {-where-}
    {-sig = fromJust $ decode signatureBS :: Signature-}
    {-pubKey = toPublic $ fromJust $ decode keyBS :: PublicKey-}
verifyDataSignature signatureBS keyBS Nothing Nothing = verify pubKey BS.empty sig
  where
    -- decoded from the hex string
    -- create a signature from a bytestring
    sig = case decode signatureBS of
      Left _ -> error "invalid signature"
      Right bs -> case signature bs of
        CryptoFailed e -> error $ show e
        CryptoPassed s -> s
    {-sig = _ $ decode signatureBS :: Signature-}
    {-pubKey = toPublic $ fromJust $ decode keyBS :: PublicKey-}
    pubKey = undefined

verifyDataSignature _ _ _ _ = undefined

{-main :: IO ()-}
{-main = do-}
  {-let keyBS = fst $ decode $ BS.pack "a4010103272006215820b89526fd6bf4ba737c55ea90670d16a27f8de6cc1982349b3b676705a2f420c6"-}
  {-let signatureBS = fst $ decode $ BS.pack "84582aa201276761646472657373581de118987c1612069d4080a0eb247820cb987fea81bddeaafdd41f996281a1666861736865646f458264175677573746120416461204b696e672c20436f756e74657373206f66204c6f76656c61636558401712458b19f606b322982f6290c78529a235b56c0f1cec4f24b12a8660b40cd37f4c5440a465754089c462ed4b0d613bffaee3d1833516569fda4852f42a4a0f"-}
  {-let messageBS = Just $ BS.pack "Augusta Ada King, Countess of Lovelace"-}
  {-let addressBS = Just $ BS.pack "stake1uyvfslqkzgrf6syq5r4jg7pqewv8l65phh024lw5r7vk9qgznhyty"-}

  {-print $ verifyDataSignature signatureBS keyBS messageBS addressBS -- True-}
  {-print $ verifyDataSignature signatureBS keyBS messageBS Nothing -- True-}
  {-print $ verifyDataSignature signatureBS keyBS Nothing Nothing -- True-}
  {-print $ verifyDataSignature signatureBS keyBS messageBS (Just $ BS.pack "stake1_test1hweafkafrwf9ets85rs9gtk9qgzegwtg") -- False-}
  {-print $ verifyDataSignature signatureBS keyBS (Just $ BS.pack "Augusta Ada King, Countess of Lovelace!") -- False-}

 --------------------------------------------------------------------------------
 -- |



data CosePublicKey = CosePublicKey {
    keyType :: Int,
    algorithm :: Int,
    key :: BS.ByteString
}

decodeCosePublicKey :: LBS.ByteString -> Either String PublicKeyWithSignAlg
decodeCosePublicKey bs = case Serialise.deserialiseOrFail bs of
    Left failure -> Left (show failure)
    Right a -> Right a
