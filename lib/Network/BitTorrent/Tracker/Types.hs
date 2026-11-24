{-# LANGUAGE DuplicateRecordFields #-}

module Network.BitTorrent.Tracker.Types (Peer (..), TrackerResponse (..), Event (..)) where

import Data.ByteString (ByteString)
import Network.Socket (SockAddr)

data Peer = Peer
  { peerId :: Maybe ByteString,
    ip :: SockAddr
  }
  deriving (Show, Eq)

data TrackerResponse = TrackerResponse
  { interval :: Int,
    complete :: Maybe Int,
    incomplete :: Maybe Int,
    peers :: [Peer]
  }
  deriving (Show, Eq)

data Event = None | Completed | Started | Stopped deriving (Show, Eq)
