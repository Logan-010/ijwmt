{-# LANGUAGE TypeSynonymInstances #-}

module Network.BitTorrent.Transport.TCP (TCP (..), newTcpSocket, connectTCP, acceptTCP) where

import Control.Monad (when)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import qualified Data.ByteString as BS
import Network.BitTorrent (PeerStream (..))
import Network.Socket (Family (AF_INET), SockAddr, Socket, SocketType (Stream), accept, connect, defaultProtocol, socket)
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

data TCP = TCP {inner :: Socket, incoming :: MVar BS.ByteString}

newTcpSocket :: Socket -> IO TCP
newTcpSocket sock = TCP sock <$> newMVar BS.empty

connectTCP :: SockAddr -> IO TCP
connectTCP addr = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock addr
  newTcpSocket sock

acceptTCP :: TCP -> IO (TCP, SockAddr)
acceptTCP tcp =
  accept (inner tcp) >>= \(sock, addr) -> do
    peer <- newTcpSocket sock
    pure (peer, addr)

instance PeerStream TCP where
  readBytes tcp n = modifyMVar (incoming tcp) $ \buffered -> do
    when (n < 0) (fail "Negative read length")
    let (prefix, rest) = BS.splitAt n buffered
    bytes <- recvExactly (inner tcp) (n - BS.length prefix)
    pure (rest, prefix <> bytes)
  peekBytes tcp n = modifyMVar (incoming tcp) $ \buffered -> do
    when (n < 0) (fail "Negative peek length")
    bytes <- recvExactly (inner tcp) (max 0 (n - BS.length buffered))
    let available = buffered <> bytes
    pure (available, BS.take n available)
  writeBytes tcp = NSB.sendAll $ inner tcp
