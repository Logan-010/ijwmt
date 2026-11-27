{-# LANGUAGE OverloadedStrings #-}

module UTPSpec (tests) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, finally, throwIO, try)
import Control.Monad (foldM, forM, forM_, void)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Network.BitTorrent (PeerStream (..))
import Network.BitTorrent.Transport.UTP
import Network.Socket (Socket, SockAddr (..), Family (AF_INET), SocketType (Datagram), defaultProtocol, tupleToHostAddress)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Timeout (timeout)
import TestSupport

tests :: [Test]
tests =
  [ ("uTP: encode network-order fixture", assertEqual (Right fixture) (encodePacket packet)),
    ("uTP: local port matches bound socket", withSocket $ \sock -> do
        expected <- NS.socketPort sock
        bracket (newUTPSocket sock) closeUTPSocket $ \manager ->
          localPort manager >>= assertEqual expected),
    ("uTP: decode network-order fixture", assertEqual (Right packet) (decodePacket fixture)),
    ("uTP: extension chain", assertEqual (Right extended) (encodePacket extended >>= decodePacket)),
    ("uTP: reject invalid encoder extension", assertLeft (encodePacket packet { extensions = [(0, "")] })),
    ("uTP: reject oversized encoder extension", assertLeft (encodePacket packet { extensions = [(2, BS.replicate 256 0)] })),
    ("uTP: bidirectional multi-packet stream", bounded streamTest),
    ("uTP: retransmission, exact reads, duplicates and sequence wrap", bounded receiveTest),
    ("uTP: retransmit lost DATA acknowledgement", bounded retransmitTest),
    ("uTP: FIN and EOF", bounded finishTest),
    ("uTP: reset wakes readers", bounded resetTest),
    ("uTP: retry lost SYN response", bounded synRetryTest),
    ("uTP: accepting handshake and duplicate SYN", bounded acceptTest),
    ("uTP: respect remote window", bounded windowTest),
    ("uTP: bounded receive buffer and window reopening", bounded receiveWindowTest),
    ("uTP: cancelled write wakes blocked reader", bounded cancellationTest),
    ("uTP: multiplex concurrent connections and close isolation", bounded multiplexTest),
    ("uTP: equal IDs from different addresses", bounded equalIdsTest),
    ("uTP: closing manager wakes all blocked operations", bounded socketCloseTest),
    ("uTP: sliding window, selective ACK and fast recovery", bounded selectiveRecoveryTest),
    ("uTP: reject ACKs beyond send window", bounded invalidAckTest),
    ("uTP: out-of-order FIN waits for missing data", bounded reorderedFinTest),
    ("uTP: LEDBAT yields above target delay", bounded congestionTest),
    ("uTP: RESET can echo our send ID", bounded echoedResetTest),
    ("uTP: slow peer does not block other connections", bounded slowPeerTest),
    ("uTP: transfer through loss, reordering and duplication", bounded lossyTransferTest),
    ("uTP: cancelled connect removes its route", bounded cancelledConnectTest),
    ("uTP: one socket connects and accepts concurrently", bounded dualRoleTest),
    ("uTP: accept backlog is bounded and reusable", bounded backlogTest)
  ]
    ++ [("uTP: reject header truncation " ++ show n, assertLeft (decodePacket (BS.take n fixture))) | n <- [0 .. 19]]
    ++ [("uTP: reject malformed packet " ++ show raw, assertLeft (raw >>= decodePacket))
       | raw <-
            [ Right (BS.cons 0x02 (BS.tail fixture)),
              Right (BS.cons 0x51 (BS.tail fixture)),
              Right (BS.take 1 fixture <> BS.singleton 1 <> BS.drop 2 fixture),
              Right (BS.take 1 fixture <> BS.singleton 1 <> BS.drop 2 fixture <> BS.pack [0, 4, 1]),
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

withUTPSocket :: (UTP -> Socket -> IO a) -> IO a
withUTPSocket use = withUTPManager $ \_ handle sock -> use handle sock

withUTPManager :: (UTPSocket -> UTP -> Socket -> IO a) -> IO a
withUTPManager use = withSocket $ \sock ->
  bracket (newUTPSocket sock) closeUTPSocket $ \manager -> do
    handle <- newUTP manager
    use manager handle sock

streamTest :: IO ()
streamTest = withUTPSocket $ \server serverSocket -> withUTPSocket $ \client _ -> do
  peer <- NS.getSocketName serverSocket
  withWorker (acceptUTP server) $ \accepted -> do
    sender <- connectUTP client peer
    receiver <- accepted
    let bytes = BS.concat (replicate 40 (BS.pack [0 .. 255]))
        exchange a b body = withWorker (writeBytes a body) $ \sent -> do
          assertEqual (BS.take 37 body) =<< peekBytes b 37
          assertEqual (BS.take 17 body) =<< peekBytes b 17
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
      `finally` (abortUTP sender >> abortUTP receiver)

-- An independent UDP endpoint supplies headers rather than using our codec for
-- the SYN response: the initial STATE consumes a sequence number, unlike ACKs.
withClient :: (UTP -> Socket -> SockAddr -> Packet -> IO ()) -> IO ()
withClient use = withSocket $ \server -> withUTPSocket $ \client _ -> do
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
    confirmation <- receiveType server State
    assertEqual 65534 (acknowledgementNumber confirmation)
    use stream server source syn `finally` abortUTP stream

send :: Socket -> SockAddr -> Packet -> IO ()
send sock peer pkt = do
  raw <- either fail pure (encodePacket pkt)
  void (NSB.sendTo sock raw peer)

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
  send sock peer second
  selective <- receiveType sock State
  assertEqual 65534 (acknowledgementNumber selective)
  assertEqual [(1, BS.pack [1, 0, 0, 0])] (extensions selective)
  sendAck first 0
  sendAck first 0
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
synRetryTest = withSocket $ \server -> withUTPSocket $ \client _ -> do
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
    abortUTP stream

acceptTest :: IO ()
acceptTest = withUTPSocket $ \server serverSocket -> withSocket $ \client -> do
  peer <- NS.getSocketName serverSocket
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
        sent) `finally` abortUTP stream

windowTest :: IO ()
windowTest = withClient $ \stream sock peer syn -> do
  let state = (incomingPacket syn) { packetType = State, windowSize = 3 }
  -- A DATA packet synchronizes the receiver with the small advertised window.
  send sock peer state { packetType = Data, payload = "x" }
  assertEqual "x" =<< readBytes stream 1
  ackNr <- withWorker (writeBytes stream "abcdefgh") $ \sent -> do
    (chunks, lastSequence) <- foldM (\(parts, _) _ -> do
      body <- receiveType sock Data
      assertEqual (if length parts == 2 then 2 else 3) (BS.length (payload body))
      send sock peer state { acknowledgementNumber = sequenceNumber body }
      pure (parts ++ [payload body], sequenceNumber body)) ([], sequenceNumber syn) [1 :: Int .. 3]
    assertEqual "abcdefgh" (BS.concat chunks)
    sent
    pure lastSequence
  -- A zero-window peer must still be probed, so a lost window update cannot
  -- deadlock the stream indefinitely.
  send sock peer state { packetType = Data, sequenceNumber = 0, payload = "x", windowSize = 0, acknowledgementNumber = ackNr }
  assertEqual "x" =<< readBytes stream 1
  withWorker (writeBytes stream "zyx") $ \sent -> do
    probe <- receiveType sock Data
    assertEqual "z" (payload probe)
    send sock peer state { acknowledgementNumber = sequenceNumber probe, windowSize = 0 }
    early <- timeout 100000 (receiveType sock Data)
    assertEqual Nothing early
    send sock peer state { acknowledgementNumber = sequenceNumber probe }
    rest <- receiveType sock Data
    assertEqual "yx" (payload rest)
    send sock peer state { acknowledgementNumber = sequenceNumber rest }
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

withWorkers :: [IO a] -> ([IO a] -> IO b) -> IO b
withWorkers [] use = use []
withWorkers (action : rest) use = withWorker action $ \result ->
  withWorkers rest (use . (result :))

assertIOFailure :: IO a -> IO ()
assertIOFailure action = do
  result <- try (void action) :: IO (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right () -> fail "Expected an IO exception"

multiplexTest :: IO ()
multiplexTest = withUTPSocket $ \server serverSocket -> withUTPSocket $ \client _ -> do
  peer <- NS.getSocketName serverSocket
  pairs <- forM [1 :: Int .. 8] $ \i -> do
    sender <- connectUTP client peer
    receiver <- acceptUTP server
    pure (i, sender, receiver)
  let exchange (i, sender, receiver) = do
        let body = BS.replicate (50000 + i) (fromIntegral i)
        withWorker (writeBytes sender body) $ \sent -> do
          assertEqual body =<< readBytes receiver (BS.length body)
          sent
        withWorker (writeBytes receiver (BS.reverse body)) $ \sent -> do
          assertEqual (BS.reverse body) =<< readBytes sender (BS.length body)
          sent
  withWorkers (map exchange pairs) sequence_
  case pairs of
    (_, sender, receiver) : rest -> do
      closeUTP sender
      assertIOFailure (readBytes receiver 1)
      -- The closed sender's TIME-WAIT route must ACK this later FIN.
      closeUTP receiver
      closeUTP sender
      withWorkers (map exchange rest) sequence_
      next <- connectUTP client peer
      accepted <- acceptUTP server
      writeBytes next "still listening"
      assertEqual "still listening" =<< readBytes accepted 15
      abortUTP next
      abortUTP accepted
    [] -> fail "No streams created"

equalIdsTest :: IO ()
equalIdsTest = withUTPSocket $ \server serverSocket -> withSocket $ \a -> withSocket $ \b -> do
  peer <- NS.getSocketName serverSocket
  let syn = Packet Syn 65535 0 0 1048576 1 0 [] BS.empty
      open sock = do
        send sock peer syn
        state <- receiveType sock State
        accepted <- acceptUTP server
        send sock peer syn
          { packetType = Data, connectionId = 0, sequenceNumber = 2,
            acknowledgementNumber = sequenceNumber state, payload = if sock == a then "a" else "b" }
        pure accepted
  first <- open a
  second <- open b
  assertEqual "a" =<< readBytes first 1
  assertEqual "b" =<< readBytes second 1
  abortUTP first
  send b peer syn { packetType = Data, connectionId = 0, sequenceNumber = 3, payload = "c" }
  assertEqual "c" =<< readBytes second 1
  abortUTP second

socketCloseTest :: IO ()
socketCloseTest = withUTPManager $ \manager server serverSocket -> withUTPSocket $ \client _ -> do
  peer <- NS.getSocketName serverSocket
  sender <- connectUTP client peer
  receiver <- acceptUTP server
  withWorker (acceptUTP server) $ \accepted ->
    withWorker (readBytes receiver 1) $ \reading ->
      withWorker (writeBytes receiver (BS.replicate (2 * 1024 * 1024) 42)) $ \writing -> do
        closeUTPSocket manager
        assertIOFailure accepted
        assertIOFailure reading
        assertIOFailure writing
        assertIOFailure (connectUTP server peer)
        closeUTPSocket manager
  abortUTP sender

selectiveRecoveryTest :: IO ()
selectiveRecoveryTest = withClient $ \stream sock peer syn ->
  withWorker (writeBytes stream (BS.concat (replicate 200 (BS.pack [0 .. 255])))) $ \_ -> do
    let ack seqNr exts = send sock peer (incomingPacket syn)
          { packetType = State, acknowledgementNumber = seqNr, extensions = exts }
        batch count = forM [1 :: Int .. count] (const (receiveType sock Data))
    first <- receiveType sock Data
    second <- receiveType sock Data
    assertEqual (sequenceNumber first + 1) (sequenceNumber second)
    ack (sequenceNumber second) []
    warm <- batch 3
    let warmAck = foldl (\_ pkt -> sequenceNumber pkt) (sequenceNumber second) warm
    ack warmAck []
    hole <- receiveType sock Data
    later <- batch 3
    -- Three later packets are SACKed without moving the cumulative ACK.
    ack warmAck [(1, BS.pack [7, 0, 0, 0])]
    retried <- timeout 250000 (receiveType sock Data)
    case retried of
      Just retry -> do
        assertEqual (sequenceNumber hole) (sequenceNumber retry)
        assertEqual (payload hole) (payload retry)
      Nothing -> fail "Fast recovery waited for the retransmission timeout"
    -- Drop the hole's retry too. SACKed packets must not be retransmitted.
    -- There may be newly permitted data before the timeout retry.
    let waitRetry = do
          next <- receiveType sock Data
          if sequenceNumber next == sequenceNumber hole then pure () else do
            forM_ later $ \sacked ->
              if sequenceNumber next == sequenceNumber sacked then fail "Retransmitted a SACKed packet" else pure ()
            waitRetry
    waitRetry
    abortUTP stream

invalidAckTest :: IO ()
invalidAckTest = withClient $ \stream sock peer syn ->
  withWorker (writeBytes stream "payload") $ \sent -> do
    body <- receiveType sock Data
    send sock peer (incomingPacket syn)
      { packetType = State, acknowledgementNumber = sequenceNumber body + 100 }
    early <- timeout 100000 sent
    assertEqual Nothing early
    send sock peer (incomingPacket syn)
      { packetType = State, acknowledgementNumber = sequenceNumber body }
    sent

reorderedFinTest :: IO ()
reorderedFinTest = withClient $ \stream sock peer syn -> do
  let packetAt seqNr kind body = (incomingPacket syn)
        { sequenceNumber = seqNr, packetType = kind, payload = body }
  send sock peer (packetAt 1 Fin BS.empty)
  finAck <- receiveType sock State
  assertEqual 65534 (acknowledgementNumber finAck)
  assertEqual [(1, BS.pack [2, 0, 0, 0])] (extensions finAck)
  send sock peer (packetAt 0 Data "def")
  void (receiveType sock State)
  send sock peer (packetAt 2 Data "not part of stream")
  void (receiveType sock State)
  send sock peer (packetAt 65535 Data "abc")
  finalAck <- receiveType sock State
  assertEqual 1 (acknowledgementNumber finalAck)
  assertEqual "abcdef" =<< readBytes stream 6
  assertIOFailure (readBytes stream 1)

congestionTest :: IO ()
congestionTest = withClient $ \stream sock peer syn ->
  withWorker (writeBytes stream (BS.replicate 100000 42)) $ \_ -> do
    let roundTrip (count, delay) = do
          packets <- forM [1 :: Int .. count] (const (receiveType sock Data))
          let ackNr = foldl (\_ pkt -> sequenceNumber pkt) (sequenceNumber syn) packets
          send sock peer (incomingPacket syn)
            { packetType = State, acknowledgementNumber = ackNr, timestampDifference = delay }
    -- Grow 2 -> 3 -> 4 -> 5 MTUs, then exhaust the three-sample delay filter.
    mapM_ roundTrip [(2, 1000000), (3, 2000000), (4, 2000000), (5, 2000000)]
    early <- timeout 100000 (receiveType sock Data)
    assertEqual Nothing early
    restarted <- receiveType sock Data
    assertEqual 130 (BS.length (payload restarted))
    abortUTP stream

echoedResetTest :: IO ()
echoedResetTest = withClient $ \stream sock peer syn ->
  withWorker (writeBytes stream "reset me") $ \sent -> do
    body <- receiveType sock Data
    send sock peer (incomingPacket syn)
      { packetType = Reset, connectionId = connectionId body,
        acknowledgementNumber = sequenceNumber body }
    assertIOFailure sent

slowPeerTest :: IO ()
slowPeerTest = withUTPSocket $ \server serverSocket -> withUTPSocket $ \client _ -> do
  peer <- NS.getSocketName serverSocket
  slow <- connectUTP client peer
  unread <- acceptUTP server
  fast <- connectUTP client peer
  reader <- acceptUTP server
  withWorker (writeBytes slow (BS.replicate (2 * 1024 * 1024) 42)) $ \writing -> do
    writeBytes fast "not blocked"
    assertEqual "not blocked" =<< readBytes reader 11
    abortUTP slow
    assertIOFailure writing
    writeBytes fast "still connected"
    assertEqual "still connected" =<< readBytes reader 15
  abortUTP unread
  abortUTP fast
  abortUTP reader

lossyTransferTest :: IO ()
lossyTransferTest = withUTPSocket $ \server serverSocket -> withUTPSocket $ \client clientSocket -> withSocket $ \proxy -> do
  serverAddress <- NS.getSocketName serverSocket
  clientAddress <- NS.getSocketName clientSocket
  proxyAddress <- NS.getSocketName proxy
  -- Reorder the first pair in each direction, drop selected DATA packets once,
  -- and duplicate others. A timer flushes an unpaired packet at low cwnds.
  let forward target bytes = void (NSB.sendTo proxy bytes target)
      loop held swapped dropped count = do
        next <- timeout 50000 (NSB.recvFrom proxy 65535)
        case next of
          Nothing -> do
            forM_ (Map.elems held) $ \(target, bytes) -> forward target bytes
            loop Map.empty (Set.union swapped (Map.keysSet held)) dropped count
          Just (raw, source) -> do
            let target = if source == clientAddress then serverAddress else clientAddress
            case decodePacket raw of
              Right pkt | packetType pkt == Data -> do
                let key = (source, sequenceNumber pkt)
                if not (Set.member source swapped)
                  then case Map.lookup source held of
                    Nothing -> loop (Map.insert source (target, raw) held) swapped dropped count
                    Just (oldTarget, old) -> do
                      forward target raw
                      forward oldTarget old
                      loop (Map.delete source held) (Set.insert source swapped) dropped count
                  else if count `mod` (11 :: Int) == 0 && not (Set.member key dropped)
                    then loop held swapped (Set.insert key dropped) (count + 1)
                    else do
                      forward target raw
                      if count `mod` 7 == 0 then forward target raw else pure ()
                      loop held swapped dropped (count + 1)
              _ -> forward target raw >> loop held swapped dropped count
  withWorker (loop Map.empty Set.empty Set.empty 1) $ \_ -> do
    sender <- connectUTP client proxyAddress
    receiver <- acceptUTP server
    let bytes = BS.concat (replicate 500 (BS.pack [0 .. 255]))
        transfer a b body = withWorker (writeBytes a body) $ \sent -> do
          assertEqual body =<< readBytes b (BS.length body)
          sent
    transfer sender receiver bytes
    transfer receiver sender (BS.reverse bytes)
    closeUTP sender
    closeUTP receiver

cancelledConnectTest :: IO ()
cancelledConnectTest = withUTPSocket $ \manager local -> withSocket $ \remote -> do
  peer <- NS.getSocketName remote
  localAddress <- NS.getSocketName local
  done <- newEmptyMVar
  bracket (forkFinally (connectUTP manager peer) (putMVar done)) killThread $ \connecting -> do
    syn <- receiveType remote Syn
    killThread connecting
    result <- readMVar done
    case result of
      Left _ -> pure ()
      Right _ -> fail "Expected cancelled connect"
    void (receiveType remote Reset)
    -- Reuse the old receive route for a passive connection. A stale active
    -- route or reservation would swallow/reject this SYN.
    let incomingSyn = syn { connectionId = connectionId syn - 1 }
    send remote localAddress incomingSyn
    state <- receiveType remote State
    accepted <- acceptUTP manager
    send remote localAddress incomingSyn
      { packetType = Data, connectionId = connectionId syn, sequenceNumber = 2,
        acknowledgementNumber = sequenceNumber state, payload = "usable" }
    assertEqual "usable" =<< readBytes accepted 6
    abortUTP accepted

dualRoleTest :: IO ()
dualRoleTest = withUTPSocket $ \a aSocket -> withUTPSocket $ \b bSocket -> do
  aAddress <- NS.getSocketName aSocket
  bAddress <- NS.getSocketName bSocket
  outgoingA <- connectUTP a bAddress
  incomingB <- acceptUTP b
  outgoingB <- connectUTP b aAddress
  incomingA <- acceptUTP a
  assertEqual aAddress (peerAddress incomingB)
  assertEqual bAddress (peerAddress incomingA)
  writeBytes outgoingA "one"
  writeBytes outgoingB "two"
  assertEqual "one" =<< readBytes incomingB 3
  assertEqual "two" =<< readBytes incomingA 3
  mapM_ closeUTP [outgoingA, incomingB, outgoingB, incomingA]

backlogTest :: IO ()
backlogTest = withUTPSocket $ \server serverSocket -> withSocket $ \client -> do
  peer <- NS.getSocketName serverSocket
  let syn cid = Packet Syn cid 0 0 1048576 1 0 [] BS.empty
  forM_ [0 :: Int .. 127] $ \i -> do
    send client peer (syn (fromIntegral (2 * i)))
    void (receiveType client State)
  send client peer (syn 256)
  rejected <- receiveType client Reset
  assertEqual 256 (connectionId rejected)
  accepted <- acceptUTP server
  send client peer (syn 256)
  state <- receiveType client State
  assertEqual 256 (connectionId state)
  abortUTP accepted
