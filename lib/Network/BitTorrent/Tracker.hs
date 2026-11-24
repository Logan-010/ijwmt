module Network.BitTorrent.Tracker (module Network.BitTorrent.Tracker.Types, Tracker (..), new, announce) where

import Data.ByteString (ByteString)
import qualified Network.BitTorrent.Tracker.HTTP as HTTP
import Network.BitTorrent.Tracker.Types
import qualified Network.BitTorrent.Tracker.UDP as UDP
import Network.HTTP.Client (Manager)
import Network.Socket (PortNumber)
import Network.URL (Host (protocol), URL (url_type), URLType (Absolute))
import qualified Network.URL as URL

data Tracker = HTTP (Manager, URL) | UDP UDP.TrackerRequestData 

new :: Manager -> URL -> ByteString -> ByteString -> ByteString -> PortNumber -> Int -> Int -> Int -> Event -> Bool -> Int -> Maybe Tracker
new manager base infoHash peer identifier port uploaded downloaded left event compact numwant =
  case url_type base of
    Absolute h -> case protocol h of
      URL.HTTP _ -> Just (HTTP (manager, HTTP.encode base infoHash identifier port uploaded downloaded left event compact numwant))
      URL.RawProt "udp" -> Just (UDP (UDP.TrackerRequestData h infoHash peer downloaded left uploaded event numwant port))
      _ -> Nothing
    _ -> Nothing

announce :: Tracker -> IO TrackerResponse
announce tracker = case tracker of
  HTTP (manager, url) -> HTTP.announce manager url
  UDP req -> UDP.announce req
