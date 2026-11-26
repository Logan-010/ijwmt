{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.PeerPool (connectToPeer, acceptTCPTask, acceptUTPTask, offloadPeer) where

import Control.Concurrent.Async (async)
import Control.Exception (try)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Network.BitTorrent (AnyPeerStream (..))
import Network.BitTorrent.Transport.MSE (initiate, respond)
import Network.BitTorrent.Transport.TCP (TCP, acceptTCP, connectTCP)
import Network.BitTorrent.Transport.UTP (UTP, acceptUTP, connectUTP)
import Network.Socket (SockAddr)

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

connectToPeer :: UTP -> ByteString -> SockAddr -> IO AnyPeerStream
connectToPeer utp infoHash addr =
  openStreams utp addr >>= \stream ->
    try (initiate infoHash stream) >>= \case
      Left (_ :: IOError) -> openStreams utp addr
      Right m -> pure $ AnyPeerStream m

acceptTCPTask :: ByteString -> TCP -> IO ()
acceptTCPTask infoHash tcp =
  void . async . forever $
    acceptTCP tcp >>= \(peer, _) ->
      respond infoHash peer >>= \case
        Nothing -> offloadPeer $ AnyPeerStream peer
        Just mse -> offloadPeer $ AnyPeerStream mse

acceptUTPTask :: ByteString -> UTP -> IO ()
acceptUTPTask infoHash utp =
  void . async . forever $
    acceptUTP utp >>= \peer ->
      respond infoHash peer >>= \case
        Nothing -> offloadPeer $ AnyPeerStream peer
        Just mse -> offloadPeer $ AnyPeerStream mse

offloadPeer :: AnyPeerStream -> IO ()
offloadPeer stream = pure ()
