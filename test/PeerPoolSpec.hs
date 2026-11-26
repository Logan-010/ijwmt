{-# LANGUAGE OverloadedStrings #-}

module PeerPoolSpec (tests) where

import Capabilities (reserved)
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_, void, when)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.BitTorrent.Wire (decodeHandshake, encodeHandshake)
import qualified Data.ByteString as BS
import Network.BitTorrent (AnyPeerStream (..), PeerStream (..))
import Network.BitTorrent.Transport.MSE (initiate, respond)
import Network.BitTorrent.Transport.TCP (TCP (..), connectTCP, newTcpSocket)
import Network.BitTorrent.Transport.UTP (UTP, acceptUTP, closeUTP, closeUTPSocket, newUTP, newUTPSocket)
import Network.PeerPool
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Timeout (timeout)
import TestSupport

tests :: [Test]
tests =
  [ ("Peer pool: shared listener routes plaintext torrents", bounded (routing False)),
    ("Peer pool: shared listener routes encrypted torrents", bounded (routing True)),
    ("Peer pool: slow peers do not block accepting", bounded slowPeer),
    ("Peer pool: rejects unknown, removed and mismatched torrents", bounded rejectedPeers),
    ("Peer pool: outgoing encrypted handshake", bounded (outgoing False (handshake firstHash) True)),
    ("Peer pool: outgoing plaintext fallback handshake", bounded (outgoing True (handshake firstHash) True)),
    ("Peer pool: outgoing rejects mismatched handshake", bounded (outgoing False (handshake secondHash) False)),
    ("Peer pool: outgoing rejects malformed handshake", bounded (outgoing False (BS.replicate 68 0) False))
  ]

bounded :: IO () -> IO ()
bounded action = timeout 5000000 action >>= maybe (fail "Peer pool test timed out") pure

firstHash :: BS.ByteString
firstHash = SHA1.hash "first torrent"

secondHash :: BS.ByteString
secondHash = SHA1.hash "second torrent"

handshake :: BS.ByteString -> BS.ByteString
handshake infoHash = encodeHandshake (BS.replicate 8 0) infoHash (BS.replicate 20 1)

withPool :: (PeerPool -> NS.SockAddr -> IO a) -> IO a
withPool use =
  bracket (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol) NS.close $ \socket -> do
    NS.bind socket (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
    NS.listen socket 16
    address <- NS.getSocketName socket
    tcp <- newTcpSocket socket
    pool <- newPeerPool
    bracket (acceptTCPTask pool tcp) cancel $ \_ -> use pool address

withPeer :: NS.SockAddr -> (TCP -> IO a) -> IO a
withPeer address = bracket (connectTCP address) (NS.close . inner)

assertClosed :: TCP -> IO ()
assertClosed peer = do
  result <- try (NSB.recv (inner peer) 1) :: IO (Either IOException BS.ByteString)
  case result of
    Left _ -> pure ()
    Right bytes -> assertEqual BS.empty bytes

routing :: Bool -> IO ()
routing encrypted = withPool $ \pool address -> do
  received <- newEmptyMVar
  forM_ [firstHash, secondHash] $ \infoHash ->
    registerTorrent pool infoHash $ \stream -> do
      bytes <- readBytes stream 7
      putMVar received (infoHash, bytes)
  forM_ [secondHash, firstHash] $ \infoHash ->
    withPeer address $ \peer -> do
      let exchange stream = do
            writeBytes stream (handshake infoHash <> "payload")
            (_, hash, peerId) <- readBytes stream 68 >>= either fail pure . decodeHandshake
            assertEqual infoHash hash
            assertEqual 20 (BS.length peerId)
      if encrypted
        then initiate infoHash peer >>= exchange
        else exchange peer
      assertEqual (infoHash, "payload") =<< takeMVar received

slowPeer :: IO ()
slowPeer = withPool $ \pool address -> do
  received <- newEmptyMVar
  registerTorrent pool firstHash $ \stream -> readBytes stream 7 >>= putMVar received
  withPeer address $ \_ ->
    withPeer address $ \peer -> do
      writeBytes peer (handshake firstHash <> "payload")
      void (readBytes peer 68)
      assertEqual "payload" =<< takeMVar received

rejectedPeers :: IO ()
rejectedPeers = withPool $ \pool address -> do
  received <- newEmptyMVar
  let handler stream = writeBytes stream "ok" >> putMVar received ()
      rejected infoHash = withPeer address $ \peer -> do
        writeBytes peer (handshake infoHash)
        assertClosed peer
  registerTorrent pool firstHash handler
  rejected secondHash
  unregisterTorrent pool firstHash
  rejected firstHash
  registerTorrent pool firstHash handler
  withPeer address $ \peer -> do
    stream <- initiate firstHash peer
    writeBytes stream (handshake secondHash)
    assertClosed peer
  withPeer address $ \peer -> do
    writeBytes peer (handshake firstHash)
    void (readBytes peer 68)
    assertEqual "ok" =<< readBytes peer 2
    takeMVar received

withUTP :: (UTP -> NS.SockAddr -> IO a) -> IO a
withUTP use =
  bracket (NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol) NS.close $ \socket -> do
    NS.bind socket (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
    address <- NS.getSocketName socket
    bracket (newUTPSocket socket) closeUTPSocket $ \manager -> do
      utp <- newUTP manager
      use utp address

outgoing :: Bool -> BS.ByteString -> Bool -> IO ()
outgoing fallback reply valid = withUTP $ \server address -> withUTP $ \client _ -> do
  pool <- newPeerPool
  received <- newEmptyMVar
  registerTorrent pool firstHash $ \stream -> readBytes stream 7 >>= putMVar received
  let serve = do
        when fallback $
          bracket (acceptUTP server) closeUTP $
            \peer -> void (readBytes peer 96)
        bracket (acceptUTP server) closeUTP $ \peer -> do
          stream <-
            if fallback
              then pure $ AnyPeerStream peer
              else do
                result <- respond (\identifier -> pure $ if identifier == SHA1.hash ("req2" <> firstHash) then Just firstHash else Nothing) peer
                maybe (fail "Expected MSE connection") (pure . AnyPeerStream . snd) result
          (flags, hash, peerId) <- readBytes stream 68 >>= either fail pure . decodeHandshake
          assertEqual reserved flags
          assertEqual firstHash hash
          assertEqual 20 (BS.length peerId)
          writeBytes stream (reply <> "payload")
  withAsync serve $ \worker -> do
    result <- try (connectToPeer pool client firstHash address) :: IO (Either IOException ())
    wait worker
    if valid
      then do
        either (fail . show) pure result
        assertEqual "payload" =<< takeMVar received
      else do
        case result of
          Left _ -> pure ()
          Right () -> fail "Expected handshake rejection"
        assertEqual Nothing =<< tryTakeMVar received
