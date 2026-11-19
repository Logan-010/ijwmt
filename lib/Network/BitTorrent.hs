module Network.BitTorrent (PeerStream (..)) where

import qualified Data.ByteString as BS

class PeerStream a where
  readBytes :: a -> Int -> IO BS.ByteString
  writeBytes :: a -> BS.ByteString -> IO ()
