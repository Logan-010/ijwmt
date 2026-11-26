module Capabilities (reserved) where

import Data.ByteString (ByteString)
import Data.BitTorrent.Wire (generateReserved, ReservedFlag(..))

reserved :: ByteString
reserved = generateReserved [FastExtension, ExtensionProtocol, BitTorrentV2]
