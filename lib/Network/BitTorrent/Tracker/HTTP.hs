module Network.BitTorrent.Tracker.HTTP where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (urlEncode)
import Network.Socket (PortNumber)
import Network.URL (URL (url_params), exportURL)

{-
  info_hash	Percent-encoded 20 bytes	Required
  peer_id	Percent-encoded 20 bytes	Required
  port	Decimal integer	Required, listening port
  uploaded	Decimal integer	Required, bytes uploaded
  downloaded	Decimal integer	Required, bytes downloaded
  left	Decimal integer	Required, bytes remaining
  event	ASCII string	Optional: started, completed, stopped
  compact	0 or 1	Widely supported; request compact peers
  numwant	Decimal integer	Widely supported; desired peer count
  key	Tracker-specific token	Used for identifying a client across announces
  trackerid	Opaque string	Echo tracker-provided identifier
  no_peer_id	0 or 1	Legacy noncompact response preference
  ip	IP address or hostname	Optional, usually best omitted
  ipv4, ipv6	IP addresses	Legacy, discouraged by BEP 7
-}

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

announce :: URL -> IO ByteString
announce url = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ exportURL url
  res <- httpLbs req manager
  pure $ LBS.toStrict $ responseBody res
