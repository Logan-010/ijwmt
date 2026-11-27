{-# LANGUAGE OverloadedStrings #-}

module Network.BitTorrent.Wire where

import Control.Monad (when)

import Data.BitTorrent.Wire
  ( Message
  , encodeMessage
  , decodeMessage
  , unpackInt
  )

import Network.BitTorrent (PeerStream(..))

maxMessageLength :: Int
maxMessageLength = 2 * 1024 * 1024

recvMessage :: PeerStream a => a -> IO Message
recvMessage stream = do
  lenBytes <- readBytes stream 4

  let len = unpackInt lenBytes

  when (len < 0 || len > maxMessageLength) $
    fail "Invalid BitTorrent message length"

  payload <- readBytes stream len

  case decodeMessage (lenBytes <> payload) of
    Left e -> fail e
    Right msg -> pure msg

sendMessage :: PeerStream a => a -> Message -> IO ()
sendMessage stream msg =
  either fail (writeBytes stream) (encodeMessage msg)
