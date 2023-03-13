{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module IOHK.Certification.SigningVerification.CBOR
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

-- TEST ABOVE

publicKey_ = Hexa.decode "a4010103272006215820dda5ce7572d07a0b86c9b9951b081b9b67ddbd16c4e1a7df7607c7b069557009"

-- >>> fmap (Hexa.encode . unPublicKey) $ unfoldPublicKey (fromRight_ publicKey_)
-- Right "dda5ce7572d07a0b86c9b9951b081b9b67ddbd16c4e1a7df7607c7b069557009"
rawSig_ = Hexa.decode "845846a201276761646472657373583900f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc0b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe8281a166686173686564f4581c736473616461736468656c6c6f20776f726c642f2f2f617364736164584037dbb6427cfb6587bfa5ef6d999c80aacd37d3739dbfbbd597d7da1231fc06aec3aa918e48e2c10a5cabeddb588cafaba1d90ffa98206f36ca2f3dd417de050a"

-- >>> rawSig_
-- Right "\132XF\162\SOH'gaddressX9\NUL\243u\153h\227\148<\185\142T\NAK\223\164\224\208I\138*a\241\US\ENQ\RS\150\&1\140\204\252\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129\161fhashed\244X\FSsdsadasdhello world///asdsadX@7\219\182B|\251e\135\191\165\239m\153\156\128\170\205\&7\211s\157\191\187\213\151\215\218\DC21\252\ACK\174\195\170\145\142H\226\193\n\\\171\237\219X\140\175\171\161\217\SI\250\152 o6\202/=\212\ETB\222\ENQ\n"

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b

thd3 :: (a, b, c) -> c
thd3 (_,_,c) = c

fromRight_ :: Either a b -> b
fromRight_ (Right b) = b
fromRight_ (Left _) = error "fromRight: Left"

rawSigDecoded_ :: ExpectedData
rawSigDecoded_ = fromRight_ ( (decodeCBORWith decodeRawSignature . LBS.fromStrict) =<< rawSig_)


-- ADDRESS
-- >>> (Hexa.encode . fst3) rawSigDecoded_
-- "00f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc0b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe8281"

-- MESSAGE
-- >>> snd3 rawSigDecoded_
-- "sdsadasdhello world///asdsad"

-- >>> (Hexa.encode . snd3) rawSigDecoded_
-- "736473616461736468656c6c6f20776f726c642f2f2f617364736164"

-- SIGNATURE
-- >>> Hexa.encode . thd3 $ rawSigDecoded_
-- "37dbb6427cfb6587bfa5ef6d999c80aacd37d3739dbfbbd597d7da1231fc06aec3aa918e48e2c10a5cabeddb588cafaba1d90ffa98206f36ca2f3dd417de050a"


-- Encoder

encodedSignedMesaage_ :: Encoding
encodedSignedMesaage_ =
  let (address,message,_) = rawSigDecoded_
  in encodeSignedMessage address message

phex = prettyHexEnc

thex = Hexa.encode . toStrictByteString

-- >>> phex encodedSignedMesaage_
-- "\n84  # list(4)\n   6a 53 69 67 6e 61 74 75 72 65 31  # text(\"Signature1\")\n   58 46 a2 01 27 67 61 64 64 72 65 73 73 58 39 00 \n   f3 75 99 68 e3 94 3c b9 8e 54 15 df a4 e0 d0 49 \n   8a 2a 61 f1 1f 05 1e 96 31 8c cc fc 0b 89 d4 3a \n   86 1d 41 da b6 1a d7 66 b5 0f 6e 06 29 4c dc b9 \n   53 6c c2 1d 96 fe 82 81  # bytes(70)\n   40  # bytes(0)\n   58 1c 73 64 73 61 64 61 73 64 68 65 6c 6c 6f 20 \n   77 6f 72 6c 64 2f 2f 2f 61 73 64 73 61 64  # bytes(28)"

-- >>> thex encodedSignedMesaage_
-- "846a5369676e6174757265315846a201276761646472657373583900f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc0b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe828140581c736473616461736468656c6c6f20776f726c642f2f2f617364736164"
-- >>> thex encodedSignedMesaage_ == "846a5369676e6174757265315846a201276761646472657373583900f3759968e3943cb98e5415dfa4e0d0498a2a61f11f051e96318cccfc0b89d43a861d41dab61ad766b50f6e06294cdcb9536cc21d96fe828140581c736473616461736468656c6c6f20776f726c642f2f2f617364736164"
-- True

-- >>> unfoldPayload (fromRight_ rawSig_)
-- Right (
--    UnfoldedPayload
--    { address = Address {unAddress = "\NUL\243u\153h\227\148<\185\142T\NAK\223\164\224\208I\138*a\241\US\ENQ\RS\150\&1\140\204\252\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129"}
--    , message = Message {unMessage = "sdsadasdhello world///asdsad"}
--    , signature = Signature {unSignature = "7\219\182B|\251e\135\191\165\239m\153\156\128\170\205\&7\211s\157\191\187\213\151\215\218\DC21\252\ACK\174\195\170\145\142H\226\193\n\\\171\237\219X\140\175\171\161\217\SI\250\152 o6\202/=\212\ETB\222\ENQ\n"}
--    , signedMessage = SignedMessage {unSignedMessage = "\132jSignature1XF\162\SOH'gaddressX9\NUL\243u\153h\227\148<\185\142T\NAK\223\164\224\208I\138*a\241\US\ENQ\RS\150\&1\140\204\252\v\137\212:\134\GSA\218\182\SUB\215f\181\SIn\ACK)L\220\185Sl\194\GS\150\254\130\129@X\FSsdsadasdhello world///asdsad"
--    }})
