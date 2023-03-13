{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module IOHK.Certification.SignatureVerification.CBOR
  ( unfoldPayload
  , unfoldPublicKey
  , UnfoldedPayload(..)
  , Address(..)
  , Message(..)
  , Signature(..)
  , SignedMessage(..)
  , PublicKey(..)
  ) where
import Data.ByteString
  (ByteString,empty)
import Codec.CBOR.Decoding
  ( Decoder
  , decodeBool
  , decodeBytes
  , decodeInt
  , decodeListLen
  , decodeMapLen
  , decodeString
  )
import Codec.CBOR.Encoding
  ( encodeBytes
  , encodeInt
  , encodeListLen
  , encodeMapLen
  , encodeString
  , Encoding
  )
import Codec.CBOR.Pretty
  ( prettyHexEnc )
import Codec.CBOR.Write
  ( toStrictByteString )
import Control.Monad
  (unless, replicateM_, when)

import qualified Data.ByteString.Base16 as Hexa
import qualified Codec.CBOR.Read as CBOR

import qualified Data.ByteString.Lazy as LBS

newtype Address = Address { unAddress :: ByteString } deriving (Show,Eq)
newtype Message = Message { unMessage :: ByteString } deriving (Show,Eq)
newtype Signature = Signature { unSignature :: ByteString } deriving (Show,Eq)
newtype SignedMessage = SignedMessage { unSignedMessage :: ByteString } deriving (Show,Eq)
newtype PublicKey = PublicKey { unPublicKey :: ByteString } deriving (Show,Eq)

type ExpectedData = (Address,Message,Signature)

data UnfoldedPayload = UnfoldedPayload
  { address :: Address
  , message :: Message
  , signature :: Signature
  , signedMessage :: SignedMessage
  } deriving (Show)

-- | unfold a CBOR-encoded signed payload from a CIP-30 wallet
unfoldPayload :: ByteString -> Either String UnfoldedPayload
unfoldPayload bs  = case decodeCBORWith decodeRawSignature (LBS.fromStrict bs) of
  Left err -> Left $ "unfoldPayload: " ++ show err
  Right (address,message,signature) -> Right $ UnfoldedPayload
    { address
    , message
    , signature
    , signedMessage = SignedMessage $  toStrictByteString $ encodeSignedMessage address message
    }

-- | Decode a CBOR-encoded expected data value.
decodeRawSignature :: Decoder s ExpectedData
decodeRawSignature = do
  -- expect an array of 4 elements
  len <- decodeListLen
  unless (len == 4)
    $ fail $ "decodeRawSignature: expected array of 4 elements, got " ++ show len
  address <- decodeAddress'
  -- ignore hashMap
  _ <- decodeHashedMap
  -- message
  message <- decodeBytes
  signature <- decodeBytes
  pure (Address address,Message message,Signature signature)
  where
  decodeAddress' = do
  -- get bytes address
    addressBs <- decodeBytes
    case decodeCBORWith decodeAddress (LBS.fromStrict addressBs) of
      Left err -> fail err
      Right a -> pure a
  decodeHashedMap = do
    len <- decodeMapLen
    unless (len == 1)
      $ fail $ "decodeRawSignature: expected map of 1 element, got " ++ show len
    key <- decodeString
    unless ("hashed" == key)
      $ fail $ "decodeRawSignature: unexpected key " ++ show key
    bool <- decodeBool
    when bool
      $ fail "decodeRawSignature: expected {hashed: false} , got {hashed: true}"

decodeAddress :: Decoder s ByteString
decodeAddress = do
  _ <- decodeMapLen
  _ <- decodeInt
  _ <- decodeInt
  addressKey <- decodeString
  unless ("address" == addressKey)
    $ fail $ "decodeAddress: unexpected key " ++ show addressKey
  decodeBytes

decodeCBORWith :: forall a. (forall s. Decoder s a) -> LBS.ByteString -> Either String a
decodeCBORWith decoder bs = case CBOR.deserialiseFromBytes decoder bs of
    Left err -> Left $ show err
    Right (_, payload) -> Right payload

encodeSignedMessage :: Address -> Message -> Encoding
encodeSignedMessage (Address address) (Message message) =
  encodeListLen 4
  <> encodeString "Signature1"
  <> encodeBytes (toStrictByteString encodeAddress)
  <> encodeBytes empty
  <> encodeBytes message
  where
    encodeAddress = encodeMapLen 2
      <> encodeInt 1
      <> encodeInt (-8)
      <> encodeString "address"
      <> encodeBytes address

-- example of PublicKey with algorithm
-- {1: 1, 3: -8, -1: 6, -2: h'DDA5CE7572D07A0B86C9B9951B081B9B67DDBD16C4E1A7DF7607C7B069557009'}

decodePublicKeyWithAlgorithm :: Decoder s ByteString
decodePublicKeyWithAlgorithm = do
  len <- decodeMapLen
  unless (len == 4)
    $ fail $ "decodePublicKeyWithAlgorithm: expected map of 4 elements, got " ++ show len
  -- ignore 3 key-pairs and last key
  replicateM_ 7 decodeInt
  decodeBytes

unfoldPublicKey :: ByteString -> Either String PublicKey
unfoldPublicKey bs = case decodeCBORWith decodePublicKeyWithAlgorithm (LBS.fromStrict bs) of
  Left err -> Left $ "unfoldPublicKey: " ++ show err
  Right a -> Right $ PublicKey a
