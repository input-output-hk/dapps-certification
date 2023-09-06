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
newtype Signature = Signature { unSignature :: ByteString } deriving (Eq)
newtype SignedMessage = SignedMessage { unSignedMessage :: ByteString } deriving (Show,Eq)
newtype PublicKey = PublicKey { unPublicKey :: ByteString } deriving (Eq)
newtype AddressHeader = AddressHeader { unAddressHeader :: ByteString } deriving (Show,Eq)

instance Show PublicKey where
  show (PublicKey bs) = "PublicKey " ++ show (Hexa.encode bs)

instance Show Signature where
  show (Signature bs) = "Signature " ++ show (Hexa.encode bs)

type ExpectedData = (Address,Message,Signature,AddressHeader)

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
  Right (address,message,signature,addressHeader) -> Right $ UnfoldedPayload
    { address
    , message
    , signature
    , signedMessage = SignedMessage $  toStrictByteString $ encodeSignedMessage addressHeader message
    }
{-

>>> (Right bs) = Hexa.decode "A3012704583900CA852E6C0A30AD556C9CAD808139E7D9BE715656B3302A84809DFA11DA43021ED7622C3A41B136FB61468015ACFF1D1AD34463617A2743EE6761646472657373583900CA852E6C0A30AD556C9CAD808139E7D9BE715656B3302A84809DFA11DA43021ED7622C3A41B136FB61468015ACFF1D1AD34463617A2743EE"
>>>  decodeCBORWith decodeAddress (LBS.fromStrict bs)
Right "\NUL\202\133.l\n0\173Ul\156\173\128\129\&9\231\217\190qVV\179\&0*\132\128\157\250\DC1\218C\STX\RS\215b,:A\177\&6\251aF\128\NAK\172\255\GS\SUB\211Dcaz'C\238"

>>> (Right bs) = Hexa.decode "A201276761646472657373583900EEA6EEF941F733484373F7C274BE32BADB909980360EDEF283AD2B9C88D795F6A7C69398167BCF529865C8918A3DFE103653888E820B88FC"
>>>  decodeCBORWith decodeAddress (LBS.fromStrict bs)
Right "\NUL\238\166\238\249A\247\&3HCs\247\194t\190\&2\186\219\144\153\128\&6\SO\222\242\131\173+\156\136\215\149\246\167\198\147\152\SYN{\207R\152e\200\145\138=\254\DLE6S\136\142\130\v\136\252"

-}
-- | Decode a CBOR-encoded expected data value.
decodeRawSignature :: Decoder s ExpectedData
decodeRawSignature = do
  -- expect an array of 4 elements
  len <- decodeListLen
  unless (len == 4)
    $ fail $ "decodeRawSignature: expected array of 4 elements, got " ++ show len
  (address,addressBs) <- decodeAddress'
  -- ignore hashMap
  _ <- decodeHashedMap
  -- message
  message <- decodeBytes
  signature <- decodeBytes
  pure (Address address,Message message,Signature signature, AddressHeader addressBs)
  where
  decodeAddress' = do
  -- get bytes address
    addressBs <- decodeBytes
    case decodeCBORWith decodeAddress (LBS.fromStrict addressBs) of
      Left err -> fail err
      Right a -> pure (a,addressBs)
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
  len <- decodeMapLen
  unless (len == 3 || len == 2)
    $ fail $ "decodeAddress: expected map of 2 or 3 elements, got " ++ show len
  _ <- decodeInt >> decodeInt
  when (len == 3) $ do -- Lace case
    decodeInt >> decodeBytes >> pure ()

  addressKey <- decodeString
  unless ("address" == addressKey)
    $ fail $ "decodeAddress: unexpected key " ++ show addressKey
  decodeBytes

decodeCBORWith :: forall a. (forall s. Decoder s a) -> LBS.ByteString -> Either String a
decodeCBORWith decoder bs = case CBOR.deserialiseFromBytes decoder bs of
    Left err -> Left $ show err
    Right (_, payload) -> Right payload

encodeSignedMessage :: AddressHeader -> Message -> Encoding
encodeSignedMessage (AddressHeader addressBs) (Message message) =
  encodeListLen 4
  <> encodeString "Signature1"
  <> encodeBytes addressBs
  <> encodeBytes empty
  <> encodeBytes message
  --where
    {-

    encodeAddress = encodeMapLen 3
    <> encodeInt 1
    <> encodeInt (-8)
    <> encodeInt 4
    <> encodeBytes address
    <> encodeString "address"
    <> encodeBytes address

    -}

-- example of PublicKey with algorithm (NUMY)
-- {1: 1
-- , 3: -8
-- , -1: 6
-- , -2: h'D3BE7240CD3F131316A2489C609FF4D7A75732834BD120F34618AFC66A236F16'
-- }

-- example of PublicKey with algorithm (LACE)
-- {1: 1
-- , 2: h'00CA852E6C0A30AD556C9CAD808139E7D9BE715656B3302A84809DFA11DA43021ED7622C3A41B136FB61468015ACFF1D1AD34463617A2743EE'
-- , 3: -8
-- , -1: 6
-- , -2: h'D3BE7240CD3F131316A2489C609FF4D7A75732834BD120F34618AFC66A236F16'
-- }



{-

Numy case
>>> (Right bs) = Hexa.decode "A4010103272006215820D3BE7240CD3F131316A2489C609FF4D7A75732834BD120F34618AFC66A236F16"
>>>  decodeCBORWith decodePublicKeyWithAlgorithm (LBS.fromStrict bs)
Right "\211\190r@\205?\DC3\DC3\SYN\162H\156`\159\244\215\167W2\131K\209 \243F\CAN\175\198j#o\SYN"

Lace case
>>> (Right bs) = Hexa.decode "A5010102583900CA852E6C0A30AD556C9CAD808139E7D9BE715656B3302A84809DFA11DA43021ED7622C3A41B136FB61468015ACFF1D1AD34463617A2743EE03272006215820D3BE7240CD3F131316A2489C609FF4D7A75732834BD120F34618AFC66A236F16"
>>>  decodeCBORWith decodePublicKeyWithAlgorithm (LBS.fromStrict bs)
Right "\211\190r@\205?\DC3\DC3\SYN\162H\156`\159\244\215\167W2\131K\209 \243F\CAN\175\198j#o\SYN"

-}
decodePublicKeyWithAlgorithm :: Decoder s ByteString
decodePublicKeyWithAlgorithm = do
  len <- decodeMapLen
  -- 4 for NUMY, 5 for LACE
  unless (len == 4 || len == 5)
    $ fail $ "decodePublicKeyWithAlgorithm: expected map of 4 or 5 elements, got " ++ show len
  replicateM_ 2 decodeInt
  when (len == 5) $ do -- Lace case
    decodeInt >> decodeBytes >> pure ()
  replicateM_ 5 decodeInt
  decodeBytes

unfoldPublicKey :: ByteString -> Either String PublicKey
unfoldPublicKey bs = case decodeCBORWith decodePublicKeyWithAlgorithm (LBS.fromStrict bs) of
  Left err -> Left $ "unfoldPublicKey: " ++ show err
  Right a -> Right $ PublicKey a
