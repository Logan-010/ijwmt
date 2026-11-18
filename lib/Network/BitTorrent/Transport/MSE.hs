{-# LANGUAGE OverloadedStrings #-}

module Network.BitTorrent.Transport.MSE (mseStream, IncomingProtocol (..), detectProtocol) where

import qualified Data.ByteString as BS
import Network.BitTorrent (PeerStream (readBytes))

btPrefix :: BS.ByteString
btPrefix = BS.cons 19 "BitTorrent protocol"

data IncomingProtocol
  = IncomingPlaintext
  | IncomingMSE
  deriving (Eq, Show)

detectProtocol ::
  PeerStream ->
  IO
    (IncomingProtocol, BS.ByteString)
detectProtocol stream = do
  prefix <- readBytes stream 20

  let protocol =
        if prefix == btPrefix
          then IncomingPlaintext
          else IncomingMSE

  pure (protocol, prefix)

mseStream :: PeerStream -> PeerStream
mseStream stream = error "todo"
