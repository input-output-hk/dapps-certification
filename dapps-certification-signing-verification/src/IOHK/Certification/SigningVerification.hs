{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module IOHK.Certification.SigningVerification
  ( module CBOR
  , verifyCIP30Signature
  , Bech32Address(..)
  , COSEKey
  , COSESign1
  , encodeHex
  , decodeHex
  ) where

import IOHK.Certification.SigningVerification.CBOR as CBOR
  ( unfoldPayload
  , unfoldPublicKey
  , Address(..)
  , Message(..)
  , PublicKey(..)
  , Signature(..)
  , SignedMessage(..)
  , UnfoldedPayload(..)
  )
import Cardano.Address
  ( Address, unsafeMkAddress, fromBech32 )
import Data.Text
  ( Text )
import Data.ByteString
  ( ByteString, empty )
import Crypto.Error
  ( CryptoFailable(CryptoPassed, CryptoFailed) )
import Control.Applicative
  ((<|>))
import Control.Monad
  (unless)

import qualified Data.ByteString.Base16 as Hexa
import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.Hash.BLAKE2.BLAKE2b as Blake

verifySignature :: PublicKey -> SignedMessage -> Signature -> Either String Bool
verifySignature (PublicKey pubKey) (SignedMessage msg) (Signature sig) = do
    key <- case Ed25519.publicKey pubKey of
      CryptoFailed err -> Left $ "Failed to create Ed25519 public key: " <> show err
      CryptoPassed res -> pure res
    sig <- case Ed25519.signature sig of
      CryptoFailed err -> Left $ "Failed to create Ed25519 signature: " <> show err
      CryptoPassed res -> pure res
    Right $ Ed25519.verify key msg sig

type COSEKey = ByteString
type COSESign1 = ByteString

verifyCIP30Signature :: COSEKey -> COSESign1 -> Maybe Message -> Maybe Bech32Address -> Either String Bool
verifyCIP30Signature coseKey coseSign1 mMsg mBech32Address = do
  -- decode the CBOR-encoded signed payload
  UnfoldedPayload{..} <- unfoldPayload coseSign1
  -- decode the CBOR-encoded public key
  pubKey <- unfoldPublicKey coseKey
  -- verify the address
  hashVerificationResp <- verifyHashes pubKey address mBech32Address
  unless hashVerificationResp $ Left "Hash address verification failed"
  -- verify the signature
  verificationResp <- verifySignature pubKey signedMessage signature
  Right $ verificationResp
    -- if a message was provided, verify that it matches the message in the signed payload
    && maybe True (== message) mMsg

verifyHashes :: PublicKey -> CBOR.Address -> Maybe Bech32Address -> Either String Bool
verifyHashes pubKey address mBech32Address = do
  let mBech32AddressHash = bech32AddressHash <$> mBech32Address
  let pubKeyHash = publicKeyHash pubKey
  let eAddressHash = addressHash address
  case (mBech32AddressHash,eAddressHash) of
    (_,Left err) -> Left $ "Failed to extract address hash from CBOR encoded address: " <> show err
    (Just (Left err),_) -> Left $ "Failed to extract address hash from bech32 address: " <> show err
    (Nothing,Right addressHash') ->
      Right $ Hexa.encode pubKeyHash == addressHash'
    (Just (Right bech32AddressHash'),Right addressHash') ->
      Right $ Hexa.encode pubKeyHash == addressHash' && bech32AddressHash' == addressHash'

-- | Address and public key verification

newtype Bech32Address = Bech32Address { unBech32Address :: Text }
                      deriving (Show,Eq)


data HashError
  = AddressFailedToDecode
  | InspectingAddressFailed Shelley.ErrInspectAddress
  | NoHash
  deriving (Show,Eq)

publicKeyHash :: PublicKey -> ByteString
publicKeyHash (PublicKey bs)= Blake.hash 28 empty bs

bech32AddressHash :: Bech32Address -> Either HashError ByteString
bech32AddressHash (Bech32Address bs) =
  case fromBech32 bs of
    Just cAddress -> case extractHashFromAddress cAddress of
      Left err -> Left $ InspectingAddressFailed  err
      Right Nothing -> Left NoHash
      Right (Just hash) -> Right hash
    Nothing -> Left AddressFailedToDecode

addressHash :: CBOR.Address -> Either HashError ByteString
addressHash (CBOR.Address bs) =
  let address = unsafeMkAddress bs
  in case extractHashFromAddress address of
    Left err -> Left $ InspectingAddressFailed  err
    Right Nothing -> Left NoHash
    Right (Just hash) -> Right hash

extractHashFromAddress :: Cardano.Address.Address -> Either Shelley.ErrInspectAddress (Maybe ByteString)
extractHashFromAddress =
  let inspect = Shelley.eitherInspectAddress Nothing
      getHash inspectAddress = getSpendingHash inspectAddress <|> getStakeHash inspectAddress
  in fmap ((fmap . fmap) Hexa.encode getHash) . inspect

getSpendingHash :: Shelley.InspectAddress -> Maybe ByteString
getSpendingHash (Shelley.InspectAddressShelley Shelley.AddressInfo{..}) = infoSpendingKeyHash
getSpendingHash _ = Nothing

getStakeHash :: Shelley.InspectAddress -> Maybe ByteString
getStakeHash (Shelley.InspectAddressShelley Shelley.AddressInfo{..}) = infoStakeKeyHash
getStakeHash _ = Nothing

encodeHex :: ByteString -> ByteString
encodeHex = Hexa.encode

decodeHex :: ByteString -> Either String ByteString
decodeHex = Hexa.decode

-- TEST ABOVE
fromRight_ :: Either a b -> b
fromRight_ (Right b) = b
fromRight_ (Left _) = error "fromRight: Left"

publicKey_ = fromRight_ $ Hexa.decode "a4010103272006215820dda5ce7572d07a0b86c9b9951b081b9b67ddbd16c4e1a7df7607c7b069557009"
rawSig_ = fromRight_ $ Hexa.decode "845846a201276761646472657373583900f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc0b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe8281a166686173686564f4581c736473616461736468656c6c6f20776f726c642f2f2f617364736164584037dbb6427cfb6587bfa5ef6d999c80aacd37d3739dbfbbd597d7da1231fc06aec3aa918e48e2c10a5cabeddb588cafaba1d90ffa98206f36ca2f3dd417de050a"

bash32Address_ = Bech32Address "addr_test1qrehtxtguw2rewvw2s2alf8q6pyc52np7y0s285kxxxvelqt382r4psag8dtvxkhv66s7msx99xdew2ndnppm9h7s2qsrqwcqy"
wrongBash32Address_ = Bech32Address "addr_test1qraa7pzfjjg2th8g6eh226tnkh6ek7470f3de5wz35hkqmjjuexd35tr4wajgkmfqsc8ntjk3kkqr86045m8wdq654lsu3dc8g"


message_ = Message "sdsadasdhello world///asdsad"
testVerify = case unfoldPayload rawSig_ of
  Left err -> error err
  Right (UnfoldedPayload
            { signature = Signature {unSignature = sig}
            , signedMessage = SignedMessage {unSignedMessage = signedMsg}}
        ) ->
    case unfoldPublicKey publicKey_ of
      Left err -> error err
      Right (PublicKey {unPublicKey = pk}) -> do
        let pk' = PublicKey pk
        let sm = SignedMessage signedMsg
        let s = Signature sig
        verifySignature pk' sm s

-- >>> testVerify
-- Right True

-- >>> unfoldPayload rawSig_
-- Right (UnfoldedPayload {address = Address {unAddress = "\NUL\243u\153h\227\148<\185\142T\NAK\223\164\224\208I\138*a\241\US\ENQ\RS\150\&1\140\204\252\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129"}, message = Message {unMessage = "sdsadasdhello world///asdsad"}, signature = Signature {unSignature = "7\219\182B|\251e\135\191\165\239m\153\156\128\170\205\&7\211s\157\191\187\213\151\215\218\DC21\252\ACK\174\195\170\145\142H\226\193\n\\\171\237\219X\140\175\171\161\217\SI\250\152 o6\202/=\212\ETB\222\ENQ\n"}, signedMessage = SignedMessage {unSignedMessage = "\132jSignature1XF\162\SOH'gaddressX9\NUL\243u\153h\227\148<\185\142T\NAK\223\164\224\208I\138*a\241\US\ENQ\RS\150\&1\140\204\252\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129@X\FSsdsadasdhello world///asdsad"}})

-- >>> verifyCIP30Signature publicKey_ rawSig_ Nothing Nothing
-- Right True
--
-- >>> verifyCIP30Signature publicKey_ rawSig_ (Just message_) Nothing
-- Right True

-- >>> verifyCIP30Signature publicKey_ rawSig_ (Just (Message "random")) Nothing
-- Right False
-- >>> verifyCIP30Signature publicKey_ rawSig_ (Just message_) (Just bash32Address_)
-- Right True
--
-- >>> verifyCIP30Signature publicKey_ rawSig_ Nothing (Just wrongBash32Address_)
-- Left "Hash address verification failed"


hexaDecodeE :: ByteString -> ByteString
hexaDecodeE = fromRight_ . Hexa.decode

sPubKey_ :: ByteString
sPubKey_ = hexaDecodeE "a40101032720062158208cc98ca4450283413815ef1be7393a372f0971bb1bca961579667a75b854fc38"

sBech32Address_ :: Bech32Address
sBech32Address_ = Bech32Address "stake_test1uq9cn4p6scw5rk4krttkddg0dcrzjnxuh9fkessajmlg9qgle0hjd"

sRawSig_ :: ByteString
sRawSig_ = hexaDecodeE "84582aa201276761646472657373581de00b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe8281a166686173686564f4581c736473616461736468656c6c6f20776f726c642f2f2f617364736164584062e5beae155c51399532e64ab9551509b78b1c3c49d2ff29db065d0d63d48f2fdfb0bcb7bd10a279c2dd068cd34e19c1590d2c52870e0e8681de1724e037810c"

_ =
  let addrrs = addressHash $ CBOR.Address "\224\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129"
  in undefined

-- >>>  Hexa.encode "\224\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129"
-- "e00b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe8281"

-- >>> unfoldPayload sRawSig_
-- Right (UnfoldedPayload {address = Address {unAddress = "\224\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129"}, message = Message {unMessage = "sdsadasdhello world///asdsad"}, signature = Signature {unSignature = "b\229\190\174\NAK\\Q9\149\&2\230J\185U\NAK\t\183\139\FS<I\210\255)\219\ACK]\rc\212\143/\223\176\188\183\189\DLE\162y\194\221\ACK\140\211N\EM\193Y\r,R\135\SO\SO\134\129\222\ETB$\224\&7\129\f"}, signedMessage = SignedMessage {unSignedMessage = "\132jSignature1X*\162\SOH'gaddressX\GS\224\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129@X\FSsdsadasdhello world///asdsad"}})

-- >>> verifyCIP30Signature sPubKey_ sRawSig_ Nothing Nothing
-- Right True
--
-- >>> verifyCIP30Signature sPubKey_ sRawSig_ (Just message_) Nothing
-- Data constructor not in scope: None :: Maybe Bech32Address

-- >>> verifyCIP30Signature sPubKey_ sRawSig_ (Just message_) (Just sBech32Address_)
