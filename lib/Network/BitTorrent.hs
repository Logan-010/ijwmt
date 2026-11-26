{-# LANGUAGE ExistentialQuantification #-}

module Network.BitTorrent (PeerStream (..), AnyPeerStream(..)) where

import qualified Data.ByteString as BS

class PeerStream a where
  readBytes :: a -> Int -> IO BS.ByteString
  -- | Return the next bytes without consuming them. Wait for the requested
  -- count, or fail on EOF, just like an exact read.
  peekBytes :: a -> Int -> IO BS.ByteString
  writeBytes :: a -> BS.ByteString -> IO ()

data AnyPeerStream
  = forall a. (PeerStream a) => AnyPeerStream a

instance PeerStream AnyPeerStream where
  writeBytes (AnyPeerStream s) = writeBytes s
  readBytes (AnyPeerStream s) = readBytes s
  peekBytes (AnyPeerStream s) = peekBytes s
