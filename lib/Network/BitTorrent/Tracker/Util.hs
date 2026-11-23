module Network.BitTorrent.Tracker.Util (compactPeers, word16, failure, compactIPv4, compactIPv6) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word16)
import Network.BitTorrent.Tracker.Types (Peer (Peer))
import Network.Socket (SockAddr (SockAddrInet, SockAddrInet6), tupleToHostAddress, tupleToHostAddress6)

failure :: String -> IO a
failure message = ioError (userError ("Invalid tracker response: " ++ message))

word16 :: ByteString -> Int -> Word16
word16 bytes offset = fromIntegral (BS.index bytes offset) * 256 + fromIntegral (BS.index bytes (offset + 1))

compactPeers :: Int -> (ByteString -> SockAddr) -> ByteString -> IO [Peer]
compactPeers size address bytes
  | BS.length bytes `mod` size /= 0 = failure "Invalid compact peer list length"
  | otherwise = pure (go bytes)
  where
    go remaining
      | BS.null remaining = []
      | otherwise =
          let (entry, rest) = BS.splitAt size remaining
           in Peer Nothing (address entry) : go rest

compactIPv4 :: ByteString -> SockAddr
compactIPv4 bytes =
  SockAddrInet (fromIntegral (word16 bytes 4)) $
    tupleToHostAddress (BS.index bytes 0, BS.index bytes 1, BS.index bytes 2, BS.index bytes 3)

compactIPv6 :: ByteString -> SockAddr
compactIPv6 bytes =
  SockAddrInet6
    (fromIntegral (word16 bytes 16))
    0
    (tupleToHostAddress6 (word16 bytes 0, word16 bytes 2, word16 bytes 4, word16 bytes 6, word16 bytes 8, word16 bytes 10, word16 bytes 12, word16 bytes 14))
    0
