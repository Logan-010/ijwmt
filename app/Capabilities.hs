module Capabilities (reserved) where

import Data.ByteString (ByteString)
import Data.BitTorrent.Wire (generateReserved)
import Data.BitTorrent.Wire (ReservedFlag(..))

reserved :: ByteString
reserved = generateReserved [FastExtension, ExtensionProtocol, BitTorrentV2]
