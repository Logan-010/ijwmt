{-# LANGUAGE OverloadedStrings #-}

module Network.BitTorrent.Transport.MSE (MSE, IncomingProtocol (..), detectProtocol, EncryptionPolicy (..), EncryptionMode (..)) where

import qualified Data.ByteString as BS
import Network.BitTorrent (PeerStream (readBytes, writeBytes))

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

instance PeerStream a => PeerStream (MSE a) where
  readBytes mse = error "todo"
  writeBytes mse = error "todo"
