
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Plutus.Certification.Crypto2 where

import Data.ByteString (ByteString,empty)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Base16 (decode,encode)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import "cryptonite" Crypto.Error
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.Serialise as Serialise
import qualified Codec.CBOR.Read as CBOR
import Codec.CBOR.Decoding

import qualified Data.ByteString.Lazy as LBS
import Crypto.WebAuthn.Cose.PublicKeyWithSignAlg
import Crypto.WebAuthn.Cose.PublicKey
import qualified Crypto.WebAuthn.Cose.PublicKey as Cose
import qualified Crypto.WebAuthn.Cose.PublicKeyWithSignAlg as Cose
import qualified Crypto.WebAuthn.Cose.SignAlg as Cose
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import Codec.CBOR.Decoding (decodeListLen)
import Data.ByteString.Base58
import Control.Monad (unless)
import Crypto.Hash.BLAKE2.BLAKE2b as Blake
import Crypto.Hash (hash)
import Cardano.Address ( Address, unsafeMkAddress, fromBech32 )
import Cardano.Address as CAddress
import Control.Exception
    ( SomeException, displayException )
import qualified Data.Aeson as Json
--import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as BL8

import Cardano.Address
    ( Address, unsafeMkAddress )
import Cardano.Address.Derivation
    ( XPub )
import Control.Applicative
    ( optional )
import Control.Exception
    ( SomeException, displayException )
import Control.Monad.Error.Class
    ( Error )
import System.Exit
    ( die )
import System.IO
    ( stdin, stdout )
{-import Cardano.Address.Style.Shelley (InspectAddress(..), AddressInfo (AddressInfo))-}
import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Cardano.Address.Style.Byron as Byron
-- | Some cryptonite 'Hash.HashAlgorithm' type, used as a return value of 'toCryptHashECDSA'
data SomeHashAlgorithm = forall a. Hash.HashAlgorithm a => SomeHashAlgorithm a


fromRight (Left err) =  error $ show err
fromRight (Right x) = x

keyOne :: LBS.ByteString
keyOne =
  let bs = fromRight $ decode "a4010103272006215820dda5ce7572d07a0b86c9b9951b081b9b67ddbd16c4e1a7df7607c7b069557009"
  in LBS.fromStrict bs

keyOneSimplified :: ByteString
keyOneSimplified = fromRight $ decode "dda5ce7572d07a0b86c9b9951b081b9b67ddbd16c4e1a7df7607c7b069557009"

-- >>> keyOneSimplified
-- "\221\165\206ur\208z\v\134\201\185\149\ESC\b\ESC\155g\221\189\SYN\196\225\167\223v\a\199\176iUp\t"

sigOne :: ByteString
sigOne =
  let bs = fromRight $ decode "37dbb6427cfb6587bfa5ef6d999c80aacd37d3739dbfbbd597d7da1231fc06aec3aa918e48e2c10a5cabeddb588cafaba1d90ffa98206f36ca2f3dd417de050a"
  in bs

--msgOne :: ByteString
--msgOne = "sdsadasdhello world///asdsad"

msgOne :: ByteString
msgOne =
  -- "�jSignature1XF�\x01'gaddressX9\x00�u�h��<��T\x15ߤ��I�*a�\x1F\x05\x1E�1���\v��:�\x1DAڶ\x1A�f�\x0Fn\x06)LܹSl�\x1D����@X\x1Csdsadasdhello world///asdsad"
  let bs = fromRight $ decode "846a5369676e6174757265315846a201276761646472657373583900f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc0b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe828140581c736473616461736468656c6c6f20776f726c642f2f2f617364736164"
  in bs


decodeCosePublicKey :: LBS.ByteString -> Either String PublicKeyWithSignAlg
decodeCosePublicKey bs = case Serialise.deserialiseOrFail bs of
    Left failure -> Left (show failure)
    Right a -> Right a
-- >>> decodeCosePublicKey keyOne
-- Right (PublicKeyWithSignAlgInternal {publicKeyInternal = PublicKeyEdDSA {eddsaCurve = CoseCurveEd25519, eddsaX = "\221\165\206ur\208z\v\134\201\185\149\ESC\b\ESC\155g\221\189\SYN\196\225\167\223v\a\199\176iUp\t"}, signAlgInternal = CoseSignAlgEdDSA})

theKey :: PublicKeyWithSignAlg
theKey = fromRight $ decodeCosePublicKey keyOne



-- >>> theKey
-- PublicKeyWithSignAlgInternal {publicKeyInternal = PublicKeyEdDSA {eddsaCurve = CoseCurveEd25519, eddsaX = "\221\165\206ur\208z\v\134\201\185\149\ESC\b\ESC\155g\221\189\SYN\196\225\167\223v\a\199\176iUp\t"}, signAlgInternal = CoseSignAlgEdDSA}

-- >>> verify theKey msgOne sigOne
-- Right ()
--
-- >>> verifyBase keyOneSimplified msgOne sigOne
-- Right ()

-- | Verifies an asymmetric signature for a message using a
-- 'Cose.PublicKeyWithSignAlg' Returns an error if the signature algorithm
-- doesn't match. Also returns an error if the signature wasn't valid or for
-- other errors.
verify :: Cose.PublicKeyWithSignAlg -> BS.ByteString -> BS.ByteString -> Either Text ()
verify
  Cose.PublicKeyWithSignAlg
    { publicKey = Cose.PublicKey Cose.PublicKeyEdDSA {eddsaCurve = Cose.CoseCurveEd25519, ..},
      signAlg = Cose.CoseSignAlgEdDSA
    }
  msg sig = verifyBase eddsaX msg sig
verify _ _ _ = Left "Unsupported signature algorithm"


verifyBase :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Either Text ()
verifyBase eddsaX msg sig = do
    key <- case Ed25519.publicKey eddsaX of
      CryptoFailed err -> Left $ "Failed to create Ed25519 public key: " <> Text.pack (show err)
      CryptoPassed res -> pure res
    sig <- case Ed25519.signature sig of
      CryptoFailed err -> Left $ "Failed to create Ed25519 signature: " <> Text.pack (show err)
      CryptoPassed res -> pure res
    if Ed25519.verify key msg sig
      then Right ()
      else Left "EdDSA Signature invalid"

--------------------------------------------------------------------------------
-- | CBOR



-- | A CBOR decoder for (Int,Int)
decodeIntInt:: Decoder s (Int, Int)
decodeIntInt= do
  tag <- decodeListLen
  case tag of
    2 -> do
      a <- decodeInt
      b <- decodeInt
      pure (a, b)
    _ -> fail $ "decodeIntInt: unexpected tag " <> show tag

decodeIntInt':: Decoder s Int
decodeIntInt'= do
  tag <- decodeListLen
  case tag of
    2 -> decodeInt
    _ -> fail $ "decodeIntInt: unexpected tag " <> show tag

decodeCBORWith :: forall a. (forall s. Decoder s a) -> LBS.ByteString -> Either String a
decodeCBORWith decoder bs = case CBOR.deserialiseFromBytes decoder bs of
    Left err -> Left $ show err
    Right (_, payload) -> Right payload
    {-Right (bs', payload) | LBS.null bs' -> Right payload-}
                         {-| otherwise -> Left "Failed to consume all input"-}
unHex = LBS.fromStrict . fromRight . decode

decodeMsg:: Decoder s (Text,ByteString,ByteString)
decodeMsg= do
  tag <- decodeListLen
  case tag of
    4 -> do
      signatureLabel <- decodeString
      bs <- decodeBytes
      address <- case decodeCBORWith decodeAddress (LBS.fromStrict bs) of
        Left err -> fail err
        Right a -> pure a
      _ <- decodeBytes
      msg <- decodeBytes
      pure (signatureLabel,address,msg)
    _ -> fail $ "decodeIntInt: unexpected list len " <> show tag

decodeAddress :: Decoder s ByteString
decodeAddress = do
  _ <- decodeMapLen
  _ <- decodeInt
  _ <- decodeInt
  addressKey <- decodeString
  unless ("address" == addressKey)
    $ fail $ "decodeAddress: unexpected key " ++ show addressKey
  decodeBytes

-- >>> decodeCBORWith decodeMsg (LBS.fromStrict msgOne)
-- Right ("Signature1"
-- ,"\NUL\243u\153h\227\148<\185\142T\NAK\223\164\224\208I\138*a\241\US\ENQ\RS\150\&1\140\204\252\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129"
-- ,"sdsadasdhello world///asdsad")

-- >>> decodeCBORWith decodeIntInt (unHex "820102")
-- Right (1,2)
--
-- >>> decodeCBORWith decodeIntInt' (unHex "820102")
-- Right 1


--------------------------------------------------------------------------------
-- | LET'S HASH IT

--- | Hashe Publickey
publiKeyHash :: ByteString
publiKeyHash = Blake.hash 28 empty keyOneSimplified

-- >>> encode publiKeyHash
-- "f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc"

-- this
-- >>> encode "\NUL\243u\153h\227\148<\185\142T\NAK\223\164\224\208I\138*a\241\US\ENQ\RS\150\&1\140\204\252\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129"
-- "00f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc0b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe8281"

cAddress = CAddress.fromBech32 "addr_test1qrehtxtguw2rewvw2s2alf8q6pyc52np7y0s285kxxxvelqt382r4psag8dtvxkhv66s7msx99xdew2ndnppm9h7s2qsrqwcqy"

-- >>> cAddress
-- Just (Address {unAddress = "\NUL\243u\153h\227\148<\185\142T\NAK\223\164\224\208I\138*a\241\US\ENQ\RS\150\&1\140\204\252\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129"})

inspect = Shelley.eitherInspectAddress Nothing

cAddressHash =  fmap extractHashFromAddress cAddress

extractHashFromAddress = fmap ((fmap . fmap) encode getSpendingHash) . inspect
-- >>> cAddressHash
-- Just (Right (Just "f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc"))


getSpendingHash (Shelley.InspectAddressShelley Shelley.AddressInfo{..}) = infoSpendingKeyHash
getSpendingHash _ = Nothing

addressFromCbor :: Address
addressFromCbor =  unsafeMkAddress "\NUL\243u\153h\227\148<\185\142T\NAK\223\164\224\208I\138*a\241\US\ENQ\RS\150\&1\140\204\252\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129"


addressFromCborHash :: Either Shelley.ErrInspectAddress (Maybe ByteString)
addressFromCborHash = extractHashFromAddress addressFromCbor

-- >>> addressFromCborHash
-- Right (Just "f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc")

