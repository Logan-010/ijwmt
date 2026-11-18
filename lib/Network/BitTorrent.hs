module Network.BitTorrent
  ( PeerStream(..)
  , EncryptionPolicy(..)
  , EncryptionMode(..)
  ) where

import qualified Data.ByteString as BS

data PeerStream = PeerStream
  { readBytes  :: Int -> IO BS.ByteString
  , writeBytes :: BS.ByteString -> IO ()
  }

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
