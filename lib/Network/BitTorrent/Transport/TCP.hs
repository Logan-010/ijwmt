{-# LANGUAGE TypeSynonymInstances #-}

module Network.BitTorrent.Transport.TCP (TCP) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Network.BitTorrent (PeerStream (..))
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as NSB

recvExactly :: Socket -> Int -> IO BS.ByteString
recvExactly sock n = do
  when (n < 0) $
    fail "Negative read length"

  go n []
  where
    go 0 chunks =
      pure (BS.concat (reverse chunks))
    go remaining chunks = do
      chunk <- NSB.recv sock remaining

      if BS.null chunk
        then fail "Unexpected EOF"
        else
          go
            (remaining - BS.length chunk)
            (chunk : chunks)

newtype TCP = TCP { inner :: Socket }

instance PeerStream TCP where
  readBytes tcp = recvExactly $ inner tcp
  writeBytes tcp = NSB.sendAll $ inner tcp
