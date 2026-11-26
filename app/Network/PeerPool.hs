{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.PeerPool (PeerPool, newPeerPool, registerTorrent, unregisterTorrent, connectToPeer, acceptTCPTask, acceptUTPTask) where

import Capabilities (reserved)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (finally, mask, onException, try)
import Control.Monad (forever, unless, void, when)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.BitTorrent.Wire (decodeHandshake, encodeHandshake, generatePeerId)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Network.BitTorrent (AnyPeerStream (..), PeerStream (readBytes, writeBytes))
import Network.BitTorrent.Transport.MSE (initiate, respond)
import Network.BitTorrent.Transport.TCP (TCP (..), acceptTCP, connectTCP)
import Network.BitTorrent.Transport.UTP (UTP, acceptUTP, closeUTP, connectUTP)
import Network.Socket (SockAddr, close)
import System.Timeout (timeout)

type PeerHandler = AnyPeerStream -> IO ()

data PeerPool = PeerPool (TVar (Map.Map ByteString PeerHandler, Map.Map ByteString ByteString)) QSem ByteString

newPeerPool :: IO PeerPool
newPeerPool = PeerPool <$> newTVarIO (Map.empty, Map.empty) <*> newQSem 64 <*> generatePeerId

registerTorrent :: PeerPool -> ByteString -> PeerHandler -> IO ()
registerTorrent (PeerPool torrents _ _) infoHash handler = do
  unless (BS.length infoHash == 20) (fail "Info hash must be 20 bytes")
  atomically $ modifyTVar' torrents $ \(handlers, hashes) ->
    (Map.insert infoHash handler handlers, Map.insert (SHA1.hash ("req2" <> infoHash)) infoHash hashes)

unregisterTorrent :: PeerPool -> ByteString -> IO ()
unregisterTorrent (PeerPool torrents _ _) infoHash =
  atomically $ modifyTVar' torrents $ \(handlers, hashes) ->
    (Map.delete infoHash handlers, Map.delete (SHA1.hash ("req2" <> infoHash)) hashes)

openStreams :: UTP -> SockAddr -> IO AnyPeerStream
openStreams utp addr = do
  utpStream <-
    try (connectUTP utp addr) >>= \case
      Left (_ :: IOError) -> pure Nothing
      Right v -> pure $ Just v
  tcpStream <-
    if isJust utpStream
      then pure Nothing
      else
        try (connectTCP addr) >>= \case
          Left (_ :: IOError) -> pure Nothing
          Right v -> pure $ Just v

  case (tcpStream, utpStream) of
    (Just t, Nothing) -> pure $ AnyPeerStream t
    (Nothing, Just u) -> pure $ AnyPeerStream u
    (Just _, Just u) -> pure $ AnyPeerStream u
    (Nothing, Nothing) -> fail "Could not connect to peer"

connectToPeer :: PeerPool -> UTP -> ByteString -> SockAddr -> IO ()
connectToPeer pool@(PeerPool _ _ peerId) utp infoHash addr =
  openStreams utp addr >>= \stream ->
    try (initiate infoHash stream)
      >>= ( \case
              Left (_ :: IOError) -> openStreams utp addr
              Right m -> pure $ AnyPeerStream m
          )
      >>= \peer -> do
        writeBytes peer (encodeHandshake reserved infoHash peerId)
        (_, remoteHash, _) <- readBytes peer 68 >>= either fail pure . decodeHandshake
        unless (remoteHash == infoHash) (fail "Peer info hash does not match handshake")
        offloadPeer pool infoHash peer

acceptTCPTask :: PeerPool -> TCP -> IO (Async ())
acceptTCPTask pool tcp =
  acceptTask pool (fst <$> acceptTCP tcp) (close . inner)

acceptUTPTask :: PeerPool -> UTP -> IO (Async ())
acceptUTPTask pool utp =
  acceptTask pool (acceptUTP utp) closeUTP

acceptTask :: (PeerStream a) => PeerPool -> IO a -> (a -> IO ()) -> IO (Async ())
acceptTask pool@(PeerPool torrents workers peerId) acceptPeer closePeer =
  async . forever $ mask $ \restore -> do
    waitQSem workers
    peer <- restore acceptPeer `onException` signalQSem workers
    let work = do
          result <- timeout 10000000 $ do
            response <- respond (\key -> Map.lookup key . snd <$> readTVarIO torrents) peer
            let (expected, stream) = case response of
                  Nothing -> (Nothing, AnyPeerStream peer)
                  Just (infoHash, mse) -> (Just infoHash, AnyPeerStream mse)
            (_, infoHash, _) <- readBytes stream 68 >>= either fail pure . decodeHandshake
            when (maybe False (/= infoHash) expected) (fail "MSE info hash does not match handshake")
            known <- Map.member infoHash . fst <$> readTVarIO torrents
            unless known (fail "Unknown torrent info hash")
            writeBytes stream (encodeHandshake reserved infoHash peerId)
            pure (infoHash, stream)
          case result of
            Nothing -> pure ()
            Just (infoHash, stream) -> offloadPeer pool infoHash stream
        run =
          void (try (restore work) :: IO (Either IOError ()))
            `finally` (closePeer peer `finally` signalQSem workers)
    void (async run) `onException` (closePeer peer `finally` signalQSem workers)

offloadPeer :: PeerPool -> ByteString -> AnyPeerStream -> IO ()
offloadPeer (PeerPool torrents _ _) infoHash stream = do
  handler <- Map.lookup infoHash . fst <$> readTVarIO torrents
  maybe (fail "Unknown torrent info hash") ($ stream) handler
  -- TODO
  pure ()
