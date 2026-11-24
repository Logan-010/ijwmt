module Network.BitTorrent.Tracker.UDP (TrackerRequestData (..), announce) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, toLazyByteString, word16BE, word32BE, word64BE)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32, Word64)
import Network.BitTorrent.Tracker.Types (Event (..), TrackerResponse (..))
import Network.BitTorrent.Tracker.Util (compactIPv4, compactIPv6, compactPeers, failure)
import Network.Socket (AddrInfo (..), Family (..), PortNumber, Socket, SocketType (..), close, connect, defaultHints, getAddrInfo, socket)
import Network.Socket.ByteString (recv, sendAll)
import Network.URL (Host)
import qualified Network.URL as URL
import System.Entropy (getEntropy)
import System.Timeout (timeout)

data TrackerRequestData = TrackerRequestData
  { host :: Host,
    infoHash :: ByteString,
    peer :: ByteString,
    downloaded :: Int,
    left :: Int,
    uploaded :: Int,
    event :: Event,
    numWant :: Int,
    port :: PortNumber
  }
  deriving (Show, Eq)

data Request
  = Open
  | Announce Word64 TrackerRequestData Word32

unpackWord :: (Integral a) => ByteString -> a
unpackWord = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

encode :: Word32 -> Request -> ByteString
encode tid request = BL.toStrict $ toLazyByteString $ case request of
  Open -> word64BE 0x41727101980 <> word32BE 0 <> word32BE tid
  Announce connection req key ->
    word64BE connection
      <> word32BE 1
      <> word32BE tid
      <> byteString (infoHash req)
      <> byteString (peer req)
      <> word64BE (fromIntegral (downloaded req))
      <> word64BE (fromIntegral (left req))
      <> word64BE (fromIntegral (uploaded req))
      <> word32BE (case event req of None -> 0; Completed -> 1; Started -> 2; Stopped -> 3)
      <> word32BE 0
      <> word32BE key
      <> word32BE (fromIntegral (numWant req))
      <> word16BE (fromIntegral (port req))

exchange :: Socket -> Word32 -> Word32 -> ByteString -> IO ByteString
exchange sock tid action message = do
  sendAll sock message
  receive
  where
    receive = do
      bytes <- recv sock 65535
      if BS.length bytes < 8 || unpackWord (BS.take 4 (BS.drop 4 bytes)) /= tid
        then receive
        else case (unpackWord (BS.take 4 bytes) :: Word32) of
          3 -> failure (BC.unpack (BS.drop 8 bytes))
          actual
            | actual == action -> pure bytes
            | otherwise -> failure "Unexpected UDP tracker action"

decode :: Bool -> ByteString -> IO TrackerResponse
decode v6 bytes
  | BS.length bytes < 20 = failure "UDP announce response too short"
  | otherwise = do
      entries <- compactPeers (if v6 then 18 else 6) (if v6 then compactIPv6 else compactIPv4) (BS.drop 20 bytes)
      pure (TrackerResponse (field 8) (Just (field 16)) (Just (field 12)) entries)
  where
    field offset = fromIntegral (unpackWord (BS.take 4 (BS.drop offset bytes)) :: Word32)

announce :: TrackerRequestData -> IO TrackerResponse
announce req = do
  unless (BS.length (infoHash req) == 20) $ fail "Info hash must be 20 bytes"
  unless (BS.length (peer req) == 20) $ fail "Peer ID must be 20 bytes"
  unless (all (>= 0) [downloaded req, left req, uploaded req]) $ fail "Transfer counters must be non-negative"
  unless (numWant req >= -1 && toInteger (numWant req) <= 2147483647) $ fail "Invalid number of wanted peers"
  trackerPort <- case URL.port (host req) of
    Just value | value > 0 && value <= 65535 -> pure value
    _ -> fail "UDP tracker requires a valid port"
  let name = URL.host (host req)
      hostname = case name of
        '[' : rest | not (null rest) && last rest == ']' -> init rest
        _ -> name
  addresses <- getAddrInfo (Just defaultHints {addrSocketType = Datagram}) (Just hostname) (Just (show trackerPort))
  case filter (\address -> addrFamily address `elem` [AF_INET, AF_INET6]) addresses of
    [] -> fail "No UDP tracker address found"
    address : _ -> bracket (socket (addrFamily address) Datagram (addrProtocol address)) close $ \sock -> do
      connect sock (addrAddress address)
      connectTid <- randomWord
      announceTid <- randomWord
      key <- randomWord
      let retry attempt
            | attempt > 8 = fail "UDP tracker timed out"
            | otherwise = do
                let wait = 15 * (2 ^ attempt) * 1000000
                opened <- timeout wait (exchange sock connectTid 0 (encode connectTid Open))
                case opened of
                  Nothing -> retry (attempt + 1)
                  Just bytes
                    | BS.length bytes < 16 -> failure "UDP connect response too short"
                    | otherwise -> do
                        let connection = unpackWord (BS.take 8 (BS.drop 8 bytes))
                        response <- timeout wait (exchange sock announceTid 1 (encode announceTid (Announce connection req key)))
                        case response of
                          Nothing -> retry (attempt + 1)
                          Just dat -> decode (addrFamily address == AF_INET6) dat
      retry (0 :: Int)
  where
    randomWord = unpackWord <$> getEntropy 4
