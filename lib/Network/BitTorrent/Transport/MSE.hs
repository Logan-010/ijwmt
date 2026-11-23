{-# LANGUAGE OverloadedStrings #-}

module Network.BitTorrent.Transport.MSE (MSE, IncomingProtocol (..), detectProtocol, EncryptionPolicy (..), EncryptionMode (..)) where

import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.BitTorrent (PeerStream (readBytes, writeBytes))
import System.Entropy (getEntropy)
import Prelude hiding (exponent)

data EncryptionPolicy
  = PlaintextOnly
  | PreferPlaintext
  | PreferEncrypted
  | RequireEncrypted
  deriving (Eq, Show)

data EncryptionMode
  = Plaintext
  | RC4Encrypted
  deriving (Eq, Show)

btPrefix :: BS.ByteString
btPrefix = BS.cons 19 "BitTorrent protocol"

data IncomingProtocol
  = IncomingPlaintext
  | IncomingMSE
  deriving (Eq, Show)

detectProtocol ::
  (PeerStream a) =>
  a ->
  IO
    (IncomingProtocol, BS.ByteString)
detectProtocol stream = do
  prefix <- readBytes stream 20

  let protocol =
        if prefix == btPrefix
          then IncomingPlaintext
          else IncomingMSE

  pure (protocol, prefix)

newtype MSE a = MSE a

dhKeySize :: Int
dhKeySize = 96

maxPadding :: Int
maxPadding = 512

integerToFixedBytes :: Int -> Integer -> Either String ByteString
integerToFixedBytes width n
  | width < 0 = Left "negative width"
  | n < 0 = Left "negative integer"
  | n >= 256 ^ width = Left "integer does not fit"
  | otherwise =
      Right $
        BS.pack
          [ fromIntegral ((n `shiftR` (8 * i)) .&. 0xff)
          | i <- reverse [0 .. width - 1]
          ]

bytesToInteger :: ByteString -> Integer
bytesToInteger =
  BS.foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0

generatePrivateExponent :: IO Integer
generatePrivateExponent = bytesToInteger <$> getEntropy 20

powMod :: Integer -> Integer -> Integer -> Either String Integer
powMod base exponent modulus
  | modulus <= 0 = Left "invalid modulus"
  | exponent < 0 = Left "negative exponent"
  | otherwise = Right $ go (base `mod` modulus) exponent 1
  where
    go _ 0 acc = acc
    go b e acc
      | odd e =
          go
            ((b * b) `mod` modulus)
            (e `div` 2)
            ((acc * b) `mod` modulus)
      | otherwise =
          go
            ((b * b) `mod` modulus)
            (e `div` 2)
            acc

generatePublicKey :: Integer -> Integer -> Either String ByteString
generatePublicKey prime private =
  powMod 2 private prime
    >>= integerToFixedBytes dhKeySize

deriveSharedSecret :: Integer -> Integer -> ByteString -> Either String ByteString
deriveSharedSecret prime private remotePublic =
  powMod (bytesToInteger remotePublic) private prime
    >>= integerToFixedBytes dhKeySize

instance (PeerStream a) => PeerStream (MSE a) where
  readBytes mse = error "todo"
  writeBytes mse = error "todo"
