{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module IOHK.Certification.SignatureVerification
  ( module CBOR
  , verifyCIP30Signature
  , Bech32Address(..)
  , COSEKey
  , COSESign1
  , encodeHex
  , decodeHex
  , bech32AddressHash
  ) where

import IOHK.Certification.SignatureVerification.CBOR as CBOR
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

verifyEd25519Signature :: PublicKey -> SignedMessage -> Signature -> Either String Bool
verifyEd25519Signature (PublicKey pubKey) (SignedMessage msg) (Signature sig) = do
    key <- case Ed25519.publicKey pubKey of
      CryptoFailed err -> Left $ "Failed to create Ed25519 public key: " <> show err
      CryptoPassed res -> pure res
    sig' <- case Ed25519.signature sig of
      CryptoFailed err -> Left $ "Failed to create Ed25519 signature: " <> show err
      CryptoPassed res -> pure res
    Right $ Ed25519.verify key msg sig'

type COSEKey = ByteString
type COSESign1 = ByteString

-- | Address and public key verification

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
  verificationResp <- verifyEd25519Signature pubKey signedMessage signature
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
