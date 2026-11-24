{-# LANGUAGE OverloadedStrings #-}

module TrackerUDPSpec (tests) where

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, throwIO, try)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString, word32BE)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32)
import qualified Network.BitTorrent.Tracker as Tracker
import Network.BitTorrent.Tracker.Types (Event (..), Peer (..), TrackerResponse (TrackerResponse))
import qualified Network.BitTorrent.Tracker.UDP as UDP
import Network.BitTorrent.Util (initManager)
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)
import Network.URL (Host (..), Protocol (..), URL (..), URLType (..))
import System.Timeout (timeout)
import TestSupport

tests :: [Test]
tests =
  [ ("UDP IPv4 packet layout and peers", successful AF_INET peers4 expected4),
    ("UDP IPv6 packet layout and peers", successful AF_INET6 peers6 expected6),
    ("UDP empty peer list", successful AF_INET "" []),
    ("UDP tracker errors", rejected (\tid -> word 3 <> tid <> "not registered")),
    ("UDP short announce", rejected (\tid -> word 1 <> tid)),
    ("UDP malformed compact peers", rejected (\tid -> response tid "x")),
    ("UDP unexpected action", rejected (\tid -> word 2 <> tid)),
    ( "UDP retries a dropped connect request",
      withTrackerDropping True AF_INET (\tid -> response tid "") $ \req ->
        UDP.announce req >>= assertEqual (TrackerResponse 1800 (Just 7) (Just 11) [])
    )
  ]
  where
    successful family peers expected = withTracker family (\tid -> response tid peers) $ \req -> do
      -- Exercise the public dispatcher too.
      manager <- initManager
      let url = URL (Absolute (UDP.host req)) "announce" []
      case Tracker.new manager url (UDP.infoHash req) (UDP.peer req) "unused" 51413 3 1 2 Started True (-1) of
        Nothing -> fail "UDP URL rejected"
        Just tracker -> Tracker.announce tracker >>= assertEqual (TrackerResponse 1800 (Just 7) (Just 11) expected)
    rejected reply = withTracker AF_INET reply $ \req -> do
      result <- try (UDP.announce req) :: IO (Either IOException TrackerResponse)
      case result of
        Left _ -> pure ()
        Right value -> fail ("Expected failure, got " ++ show value)

word :: Word32 -> BS.ByteString
word = BL.toStrict . toLazyByteString . word32BE

response :: BS.ByteString -> BS.ByteString -> BS.ByteString
response tid peers = word 1 <> tid <> word 1800 <> word 11 <> word 7 <> peers

peers4, peers6 :: BS.ByteString
peers4 = BS.pack [192, 0, 2, 1, 0x1a, 0xe1, 203, 0, 113, 254, 255, 255]
peers6 = BS.pack [0x20, 1, 0xd, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0xc8, 0xd5]

expected4, expected6 :: [Peer]
expected4 = [Peer Nothing (SockAddrInet 6881 (tupleToHostAddress (192, 0, 2, 1))), Peer Nothing (SockAddrInet 65535 (tupleToHostAddress (203, 0, 113, 254)))]
expected6 = [Peer Nothing (SockAddrInet6 51413 0 (tupleToHostAddress6 (0x2001, 0xdb8, 0, 0, 0, 0, 0, 1)) 0)]

withTracker :: Family -> (BS.ByteString -> BS.ByteString) -> (UDP.TrackerRequestData -> IO ()) -> IO ()
withTracker = withTrackerDropping False

withTrackerDropping :: Bool -> Family -> (BS.ByteString -> BS.ByteString) -> (UDP.TrackerRequestData -> IO ()) -> IO ()
withTrackerDropping dropFirst family reply client = bracket (socket family Datagram defaultProtocol) close $ \sock -> do
  bind sock (if family == AF_INET then SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)) else SockAddrInet6 0 0 (tupleToHostAddress6 (0, 0, 0, 0, 0, 0, 0, 1)) 0)
  address <- getSocketName sock
  let serverPort = case address of
        SockAddrInet p _ -> p
        SockAddrInet6 p _ _ _ -> p
        _ -> error "Unexpected socket address"
      req = UDP.TrackerRequestData (Host (RawProt "udp") (if family == AF_INET then "127.0.0.1" else "::1") (Just (fromIntegral serverPort))) (BS.replicate 20 1) (BS.replicate 20 2) 1 2 3 Started (-1) 51413
      connection = BS.pack [0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10]
      server = do
        dropped <- if dropFirst then Just <$> recvFrom sock 65535 else pure Nothing
        (opened, remote) <- recvFrom sock 65535
        case dropped of
          Nothing -> pure ()
          Just first -> assertEqual first (opened, remote)
        assertEqual 16 (BS.length opened)
        assertEqual (BS.pack [0, 0, 4, 23, 39, 16, 25, 128] <> word 0) (BS.take 12 opened)
        -- A stale transaction must not be accepted.
        let tid = BS.drop 12 opened
            stale = BS.cons (BS.head tid + 1) (BS.tail tid)
        void (sendTo sock (word 0 <> stale <> connection) remote)
        void (sendTo sock (word 0 <> tid <> connection) remote)
        (announced, source) <- recvFrom sock 65535
        assertEqual remote source
        assertEqual 98 (BS.length announced)
        assertEqual (connection <> word 1) (BS.take 12 announced)
        assertEqual (UDP.infoHash req <> UDP.peer req) (BS.take 40 (BS.drop 16 announced))
        assertEqual (BS.pack [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3]) (BS.take 24 (BS.drop 56 announced))
        assertEqual (word 2 <> word 0) (BS.take 8 (BS.drop 80 announced))
        assertEqual (word maxBound <> BS.pack [0xc8, 0xd5]) (BS.drop 92 announced)
        void (sendTo sock (reply (BS.take 4 (BS.drop 12 announced))) remote)
  done <- newEmptyMVar
  bracket (forkFinally server (putMVar done)) killThread $ \_ -> do
    result <- timeout (if dropFirst then 20000000 else 3000000) $ do
      client req
      takeMVar done >>= either throwIO pure
    case result of
      Nothing -> fail "Local UDP test timed out"
      Just () -> pure ()
