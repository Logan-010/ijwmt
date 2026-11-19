{-# LANGUAGE OverloadedStrings #-}

module UTPSpec (tests) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, finally, throwIO, try)
import Control.Monad (foldM, void)
import qualified Data.ByteString as BS
import Network.BitTorrent (PeerStream (..))
import Network.BitTorrent.Transport.UTP
import Network.Socket (Socket, SockAddr (..), Family (AF_INET), SocketType (Datagram), defaultProtocol, tupleToHostAddress)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Timeout (timeout)
import TestSupport

tests :: [Test]
tests =
  [ ("uTP: encode network-order fixture", assertEqual fixture (encodePacket packet)),
    ("uTP: decode network-order fixture", assertEqual (Right packet) (decodePacket fixture)),
    ("uTP: extension chain", assertEqual (Right extended) (decodePacket (encodePacket extended))),
    ("uTP: reject invalid encoder extension", assertThrows (encodePacket packet { extensions = [(0, "")] })),
    ("uTP: reject oversized encoder extension", assertThrows (encodePacket packet { extensions = [(2, BS.replicate 256 0)] })),
    ("uTP: bidirectional multi-packet stream", bounded streamTest),
    ("uTP: retransmission, exact reads, duplicates and sequence wrap", bounded receiveTest),
    ("uTP: retransmit lost DATA acknowledgement", bounded retransmitTest),
    ("uTP: FIN and EOF", bounded finishTest),
    ("uTP: reset wakes readers", bounded resetTest),
    ("uTP: retry lost SYN response", bounded synRetryTest),
    ("uTP: accepting handshake and duplicate SYN", bounded acceptTest),
    ("uTP: respect remote window", bounded windowTest),
    ("uTP: bounded receive buffer and window reopening", bounded receiveWindowTest),
    ("uTP: cancelled write wakes blocked reader", bounded cancellationTest)
  ]
    ++ [("uTP: reject header truncation " ++ show n, assertLeft (decodePacket (BS.take n fixture))) | n <- [0 .. 19]]
    ++ [("uTP: reject malformed packet " ++ show raw, assertLeft (decodePacket raw))
       | raw <-
           [ BS.cons 0x02 (BS.tail fixture),
             BS.cons 0x51 (BS.tail fixture),
             BS.take 1 fixture <> BS.singleton 1 <> BS.drop 2 fixture,
             BS.take 1 fixture <> BS.singleton 1 <> BS.drop 2 fixture <> BS.pack [0, 4, 1],
             encodePacket packet { extensions = [(1, "abc")] },
             encodePacket packet { payload = BS.empty },
             encodePacket packet { packetType = State }
           ]
       ]
  where
    extended = packet { extensions = [(1, BS.pack [1, 0, 0, 128]), (42, "unknown")] }

packet :: Packet
packet = Packet Data 0x1234 0x01020304 0x05060708 0x090a0b0c 0xfffe 0xffff [] "hi"

fixture :: BS.ByteString
fixture = BS.pack [0x01, 0, 0x12, 0x34, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0xff, 0xfe, 0xff, 0xff, 104, 105]

bounded :: IO () -> IO ()
bounded action = do
  result <- timeout 8000000 action
  case result of
    Nothing -> fail "uTP test timed out"
    Just () -> pure ()

withSocket :: (Socket -> IO a) -> IO a
withSocket = bracket create NS.close
  where
    create = do
      sock <- NS.socket AF_INET Datagram defaultProtocol
      NS.bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      pure sock

withWorker :: IO a -> (IO a -> IO b) -> IO b
withWorker action use = do
  result <- newEmptyMVar
  bracket (forkFinally action (putMVar result)) killThread $ \_ ->
    use (readMVar result >>= either throwIO pure)

ignoreFailure :: IO () -> IO ()
ignoreFailure action = void (try action :: IO (Either SomeException ()))

-- Closing the underlying sockets first also lets failed test cases clean up
-- without waiting for retransmission timeouts or leaking receiver threads.
dispose :: UTP -> Socket -> IO ()
dispose stream sock = NS.close sock >> ignoreFailure (closeUTP stream)

streamTest :: IO ()
streamTest = withSocket $ \server -> withSocket $ \client -> do
  peer <- NS.getSocketName server
  withWorker (acceptUTP server) $ \accepted -> do
    sender <- connectUTP client peer
    receiver <- accepted
    let bytes = BS.concat (replicate 40 (BS.pack [0 .. 255]))
        exchange a b body = withWorker (writeBytes a body) $ \sent -> do
          first <- readBytes b 37
          rest <- readBytes b (BS.length body - 37)
          assertEqual body (first <> rest)
          sent
    (do
      assertEqual BS.empty =<< readBytes sender 0
      exchange sender receiver bytes
      exchange receiver sender (BS.reverse bytes)
      -- A blocked reader must not prevent writes/ACKs in the other direction.
      withWorker (readBytes sender 4) $ \reply -> do
        writeBytes sender "ping"
        assertEqual "ping" =<< readBytes receiver 4
        writeBytes receiver "pong"
        assertEqual "pong" =<< reply
      result <- try (readBytes sender (-1)) :: IO (Either SomeException BS.ByteString)
      case result of
        Left _ -> pure ()
        Right _ -> fail "Accepted a negative read")
      `finally` (dispose sender client >> dispose receiver server)

-- An independent UDP endpoint supplies headers rather than using our codec for
-- the SYN response: the initial STATE consumes a sequence number, unlike ACKs.
withClient :: (UTP -> Socket -> SockAddr -> Packet -> IO ()) -> IO ()
withClient use = withSocket $ \server -> withSocket $ \client -> do
  peer <- NS.getSocketName server
  withWorker (connectUTP client peer) $ \connected -> do
    (raw, source) <- NSB.recvFrom server 65535
    syn <- either fail pure (decodePacket raw)
    assertEqual Syn (packetType syn)
    assertEqual (0x41 :: Word) (fromIntegral (BS.head raw))
    let response = BS.pack [0x21, 0] <> BS.take 2 (BS.drop 2 raw)
          <> BS.replicate 8 0 <> BS.pack [0, 16, 0, 0, 255, 254]
          <> BS.take 2 (BS.drop 16 raw)
    void (NSB.sendTo server response source)
    stream <- connected
    use stream server source syn `finally` dispose stream client

send :: Socket -> SockAddr -> Packet -> IO ()
send sock peer pkt = void (NSB.sendTo sock (encodePacket pkt) peer)

receive :: Socket -> IO Packet
receive sock = do
  (raw, _) <- NSB.recvFrom sock 65535
  either fail pure (decodePacket raw)

receiveType :: Socket -> PacketType -> IO Packet
receiveType sock kind = do
  pkt <- receive sock
  if packetType pkt == kind then pure pkt else receiveType sock kind

incomingPacket :: Packet -> Packet
incomingPacket syn = Packet Data (connectionId syn) 0 0 1048576 65535 (sequenceNumber syn) [] BS.empty

receiveTest :: IO ()
receiveTest = withClient $ \stream sock peer syn -> do
  let first = (incomingPacket syn) { payload = "abc" }
      second = first { sequenceNumber = 0, payload = "def" }
      sendAck pkt expected = do
        send sock peer pkt
        ack <- receiveType sock State
        assertEqual expected (acknowledgementNumber ack)
  -- Reject a wrong connection ID and malformed packet before valid input.
  send sock peer first { connectionId = connectionId first + 10, payload = "bad" }
  void (NSB.sendTo sock "not a uTP packet" peer)
  withSocket $ \stranger -> send stranger peer first { payload = "bad" }
  sendAck second 65534
  sendAck first 65535
  sendAck first 65535
  sendAck second 0
  assertEqual "ab" =<< readBytes stream 2
  assertEqual "cdef" =<< readBytes stream 4

retransmitTest :: IO ()
retransmitTest = withClient $ \stream sock peer syn ->
  withWorker (writeBytes stream "payload") $ \sent -> do
    first <- receiveType sock Data
    retry <- receiveType sock Data
    assertEqual (sequenceNumber first) (sequenceNumber retry)
    assertEqual "payload" (payload retry)
    send sock peer (incomingPacket syn)
      { packetType = State,
        acknowledgementNumber = sequenceNumber retry
      }
    sent

finishTest :: IO ()
finishTest = withClient $ \stream sock peer syn -> do
  let fin = (incomingPacket syn) { packetType = Fin }
  send sock peer fin
  ack <- receiveType sock State
  assertEqual 65535 (acknowledgementNumber ack)
  send sock peer fin
  duplicateAck <- receiveType sock State
  assertEqual 65535 (acknowledgementNumber duplicateAck)
  result <- try (readBytes stream 1) :: IO (Either SomeException BS.ByteString)
  case result of
    Left _ -> pure ()
    Right _ -> fail "Expected EOF"
  withWorker (closeUTP stream) $ \closed -> do
    outgoing <- receiveType sock Fin
    retry <- receiveType sock Fin
    assertEqual (sequenceNumber outgoing) (sequenceNumber retry)
    send sock peer (incomingPacket syn)
      { packetType = State,
        acknowledgementNumber = sequenceNumber outgoing
      }
    closed
    closeUTP stream

resetTest :: IO ()
resetTest = withClient $ \stream sock peer syn ->
  withWorker (readBytes stream 1) $ \readResult -> do
    send sock peer (incomingPacket syn) { packetType = Reset }
    result <- try readResult :: IO (Either SomeException BS.ByteString)
    case result of
      Left _ -> pure ()
      Right _ -> fail "Expected reset"

synRetryTest :: IO ()
synRetryTest = withSocket $ \server -> withSocket $ \client -> do
  peer <- NS.getSocketName server
  withWorker (connectUTP client peer) $ \connected -> do
    first <- receiveType server Syn
    (raw, source) <- NSB.recvFrom server 65535
    retry <- either fail pure (decodePacket raw)
    assertEqual Syn (packetType retry)
    assertEqual (connectionId first) (connectionId retry)
    assertEqual (sequenceNumber first) (sequenceNumber retry)
    send server source (incomingPacket first)
      { packetType = State,
        sequenceNumber = 100,
        acknowledgementNumber = sequenceNumber first
      }
    stream <- connected
    dispose stream client

acceptTest :: IO ()
acceptTest = withSocket $ \server -> withSocket $ \client -> do
  peer <- NS.getSocketName server
  let syn = Packet Syn 65535 0 0 1048576 1 0 [] BS.empty
  withWorker (acceptUTP server) $ \accepted -> do
    send client peer syn
    state <- receiveType client State
    stream <- accepted
    (do
      assertEqual 65535 (connectionId state)
      assertEqual 1 (acknowledgementNumber state)
      send client peer syn
      duplicate <- receiveType client State
      assertEqual (sequenceNumber state) (sequenceNumber duplicate)
      withWorker (writeBytes stream "hello") $ \sent -> do
        body <- receiveType client Data
        assertEqual (sequenceNumber state + 1) (sequenceNumber body)
        assertEqual 65535 (connectionId body)
        send client peer syn
          { packetType = State,
            connectionId = 0,
            sequenceNumber = 2,
            acknowledgementNumber = sequenceNumber body
          }
        sent) `finally` dispose stream server

windowTest :: IO ()
windowTest = withClient $ \stream sock peer syn -> do
  let state = (incomingPacket syn) { packetType = State, windowSize = 3 }
  -- A DATA packet synchronizes the receiver with the small advertised window.
  send sock peer state { packetType = Data, payload = "x" }
  assertEqual "x" =<< readBytes stream 1
  withWorker (writeBytes stream "abcdefgh") $ \sent -> do
    chunks <- foldM (\parts _ -> do
      body <- receiveType sock Data
      assertEqual (if length parts == 2 then 2 else 3) (BS.length (payload body))
      send sock peer state { acknowledgementNumber = sequenceNumber body }
      pure (parts ++ [payload body])) [] [1 :: Int .. 3]
    assertEqual "abcdefgh" (BS.concat chunks)
    sent
  -- A zero-window peer must still be probed, so a lost window update cannot
  -- deadlock the stream indefinitely.
  send sock peer state { packetType = Data, sequenceNumber = 0, payload = "x", windowSize = 0 }
  assertEqual "x" =<< readBytes stream 1
  withWorker (writeBytes stream "z") $ \sent -> do
    probe <- receiveType sock Data
    assertEqual "z" (payload probe)
    send sock peer state { acknowledgementNumber = sequenceNumber probe }
    sent

receiveWindowTest :: IO ()
receiveWindowTest = withClient $ \stream sock peer syn -> do
  let body = (incomingPacket syn) { payload = BS.replicate 32768 42 }
  next <- foldM (\seqNr _ -> do
    send sock peer body { sequenceNumber = seqNr }
    ack <- receiveType sock State
    assertEqual seqNr (acknowledgementNumber ack)
    pure (seqNr + 1)) 65535 [1 :: Int .. 32]
  let extra = body { sequenceNumber = next, payload = "x" }
  send sock peer extra
  full <- receiveType sock State
  assertEqual 0 (windowSize full)
  assertEqual (next - 1) (acknowledgementNumber full)
  assertEqual (BS.singleton 42) =<< readBytes stream 1
  opened <- receiveType sock State
  assertEqual 1 (windowSize opened)
  send sock peer extra
  ack <- receiveType sock State
  assertEqual next (acknowledgementNumber ack)
  assertEqual 0 (windowSize ack)
  assertEqual (BS.replicate (1024 * 1024 - 1) 42 <> "x") =<< readBytes stream (1024 * 1024)

cancellationTest :: IO ()
cancellationTest = withClient $ \stream sock _ _ ->
  withWorker (readBytes stream 1) $ \readResult -> do
    done <- newEmptyMVar
    bracket (forkFinally (writeBytes stream "unacknowledged") (putMVar done)) killThread $ \writer -> do
      void (receiveType sock Data)
      killThread writer
      result <- readMVar done
      case result of
        Left _ -> pure ()
        Right () -> fail "Expected interrupted write"
      readFailure <- try readResult :: IO (Either SomeException BS.ByteString)
      case readFailure of
        Left _ -> pure ()
        Right _ -> fail "Expected interrupted stream"
