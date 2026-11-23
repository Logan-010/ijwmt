module Network.BitTorrent.Tracker.UDP (Request (..), announce) where

import Data.ByteString (ByteString)
import Network.BitTorrent.Tracker.Types (Event, TrackerResponse)
import Network.Socket (PortNumber)
import Network.URL (Host)

data Request = Request
  { host :: Host, infoHash :: ByteString,
    peerId :: ByteString,
    downloaded :: Int,
    left :: Int,
    uploaded :: Int,
    event :: Event,
    numWant :: Int,
    port :: PortNumber
  }
  deriving (Show, Eq)

announce :: Request -> IO TrackerResponse
announce req = error "todo"
