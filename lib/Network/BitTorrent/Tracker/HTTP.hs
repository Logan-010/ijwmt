module Network.BitTorrent.Tracker.HTTP where

import qualified Data.BitTorrent.Bencode as BE
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.Word (Word16)
import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (urlEncode)
import Network.Socket (AddrInfo (..), AddrInfoFlag (..), PortNumber, SockAddr (..), SocketType (Stream), defaultHints, getAddrInfo, tupleToHostAddress, tupleToHostAddress6)
import Network.URL (URL (url_params), exportURL)
import Network.BitTorrent.Tracker (Peer (Peer), TrackerResponse (TrackerResponse))

data Event = Started | Completed | Stopped deriving (Show, Eq)

encode :: URL -> ByteString -> ByteString -> PortNumber -> Int -> Int -> Int -> Event -> Bool -> Int -> URL
encode base infoHash peerId port uploaded downloaded left event compact numwant =
  let params =
        [ ("info_hash", BS.unpack $ urlEncode False $ BS.take 20 infoHash),
          ("peer_id", BS.unpack $ urlEncode False peerId),
          ("port", show port),
          ("uploaded", show uploaded),
          ("downloaded", show downloaded),
          ("left", show left),
          ("event", map toLower $ show event),
          ("compact", if compact then "1" else "0"),
          ("numwant", show numwant)
        ]
   in base {url_params = params}

announce :: URL -> IO TrackerResponse
announce url = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ exportURL url
  res <- httpLbs req manager
  decode $ LBS.toStrict $ responseBody res

decode :: ByteString -> IO TrackerResponse
decode body = do
  value <- either failure pure (BE.decode body)
  fields <- dictionary value
  case Map.lookup (BS.pack "failure reason") fields of
    Just (BE.BString reason) -> failure (BS.unpack reason)
    Just _ -> failure "Invalid failure reason"
    Nothing -> pure ()
  announceInterval <- required "interval" fields >>= nonNegativeInt "interval"
  seeders <- traverse (nonNegativeInt "complete") (Map.lookup (BS.pack "complete") fields)
  leechers <- traverse (nonNegativeInt "incomplete") (Map.lookup (BS.pack "incomplete") fields)
  ipv4Peers <- case Map.lookup (BS.pack "peers") fields of
    Nothing -> pure []
    Just (BE.BString bytes) -> compactPeers 6 compactIPv4 bytes
    Just (BE.BList entries) -> mapM nonCompactPeer entries
    Just _ -> failure "Invalid peers"
  ipv6Peers <- case Map.lookup (BS.pack "peers6") fields of
    Nothing -> pure []
    Just (BE.BString bytes) -> compactPeers 18 compactIPv6 bytes
    Just _ -> failure "Invalid peers6"
  pure (TrackerResponse announceInterval seeders leechers (ipv4Peers ++ ipv6Peers))
  where
    failure message = ioError (userError ("Invalid tracker response: " ++ message))

    dictionary (BE.BDict fields) = pure fields
    dictionary _ = failure "Expected dictionary"

    required key fields = maybe (failure ("Missing " ++ key)) pure (Map.lookup (BS.pack key) fields)

    nonNegativeInt :: String -> BE.BencodeValue -> IO Int
    nonNegativeInt label (BE.BInt n)
      | n >= 0 && n <= toInteger (maxBound :: Int) = pure (fromInteger n)
      | otherwise = failure ("Out-of-range " ++ label)
    nonNegativeInt label _ = failure ("Invalid " ++ label)

    nonCompactPeer entry = do
      fields <- dictionary entry
      address <- required "ip" fields >>= string "ip"
      port <- required "port" fields >>= nonNegativeInt "port"
      if port > 65535
        then failure "Out-of-range port"
        else pure ()
      identifier <- traverse (string "peer id") (Map.lookup (BS.pack "peer id") fields)
      if BS.null address || BS.elem '\0' address
        then failure "Invalid ip"
        else pure ()
      let hints = defaultHints {addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}
      addresses <- getAddrInfo (Just hints) (Just (BS.unpack address)) (Just (show port))
      case addresses of
        info : _ -> pure (Peer identifier (addrAddress info))
        [] -> failure "Invalid ip"

    string _ (BE.BString bytes) = pure bytes
    string label _ = failure ("Invalid " ++ label)

    compactPeers size address bytes
      | Bytes.length bytes `mod` size /= 0 = failure "Invalid compact peer list length"
      | otherwise = pure (go bytes)
      where
        go remaining
          | Bytes.null remaining = []
          | otherwise =
              let (entry, rest) = Bytes.splitAt size remaining
               in Peer Nothing (address entry) : go rest

    compactIPv4 bytes =
      SockAddrInet (fromIntegral (word16 bytes 4)) $
        tupleToHostAddress (Bytes.index bytes 0, Bytes.index bytes 1, Bytes.index bytes 2, Bytes.index bytes 3)

    compactIPv6 bytes =
      SockAddrInet6 (fromIntegral (word16 bytes 16)) 0
        (tupleToHostAddress6 (word16 bytes 0, word16 bytes 2, word16 bytes 4, word16 bytes 6, word16 bytes 8, word16 bytes 10, word16 bytes 12, word16 bytes 14))
        0

    word16 :: ByteString -> Int -> Word16
    word16 bytes offset = fromIntegral (Bytes.index bytes offset) * 256 + fromIntegral (Bytes.index bytes (offset + 1))
