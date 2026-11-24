module Network.BitTorrent.Tracker.HTTP (encode, decode, announce, Event (..)) where

import qualified Data.BitTorrent.Bencode as BE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import qualified Data.Map as Map
import Network.BitTorrent.Tracker.Types (Event (..), Peer (Peer), TrackerResponse (TrackerResponse))
import Network.BitTorrent.Tracker.Util (compactIPv4, compactIPv6, compactPeers, failure)
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody)
import Network.HTTP.Types (urlEncode)
import Network.Socket (AddrInfo (..), AddrInfoFlag (..), PortNumber, SocketType (Stream), defaultHints, getAddrInfo)
import Network.URL (URL (url_params), exportURL)

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

announce :: Manager -> URL -> IO TrackerResponse
announce manager url = do
  req <- parseRequest $ exportURL url
  res <- httpLbs req manager
  decode $ LBS.toStrict $ responseBody res
