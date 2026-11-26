module Network.BitTorrent.Transport.UTP
  ( UTP,
    UTPSocket,
    newUTPSocket,
    newUTP,
    closeUTPSocket,
    connectUTP,
    acceptUTP,
    closeUTP,
    abortUTP,
    peerAddress,
    PacketType (..),
    Packet (..),
    encodePacket,
    decodePacket,
  ) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, mask, onException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Bits (setBit, testBit)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.BitTorrent (PeerStream (..))
import Network.BitTorrent.Transport.UTP.Packet
import Network.Socket (SockAddr, Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Entropy (getEntropy)
import System.IO.Error (tryIOError)

type Key = (SockAddr, Word16)

-- | A shared, bound UDP socket. Exactly one dispatcher receives datagrams;
-- connections are identified by both the remote address and connection ID.
-- The sender and timer are shared too: no worker thread is needed per peer.
data UTPSocket = UTPSocket
  { socket :: Socket,
    connections :: TVar (Map.Map Key UTP),
    reservations :: TVar (Set.Set Key),
    accepting :: TBQueue UTP,
    outgoing :: TBQueue (SockAddr, Packet, Maybe UTP),
    socketFailure :: TVar (Maybe String),
    workers :: [ThreadId]
  }

data Phase = Connecting | Connected | Closing | TimeWait Word64 | Failed String
  deriving (Eq, Show)

data SentPacket = SentPacket
  { sentType :: !PacketType,
    sentBody :: !BS.ByteString,
    firstSent :: !Word64,
    lastSent :: !Word64,
    transmissions :: !Int,
    selectivelyAcked :: !Bool,
    fastRetransmitted :: !Bool
  }

data Connection = Connection
  { phase :: !Phase,
    nextSequence :: !Word16,
    lastAck :: !Word16,
    receivedSequence :: !Word16,
    sentPackets :: !(Seq (Word16, SentPacket)),
    sendBuffer :: !BS.ByteString,
    incoming :: !(Seq BS.ByteString),
    bufferedBytes :: !Int,
    reordered :: !(Map.Map Word16 (PacketType, BS.ByteString)),
    remoteFin :: !(Maybe Word16),
    remoteFinished :: !Bool,
    localFin :: !(Maybe Word16),
    remoteWindow :: !Word32,
    replyDelay :: !Word32,
    delaySamples :: ![(Word64, Word32)],
    recentDelays :: ![Word32],
    queueDelay :: !Int,
    congestionWindow :: !Double,
    lastReduction :: !Word64,
    duplicateAcks :: !Int,
    roundTripTime :: !Int,
    roundTripVariance :: !Int,
    retransmissionTimeout :: !Int,
    lastReceived :: !Word64,
    lastActivity :: !Word64,
    lastProbe :: !Word64,
    handshakeConfirmed :: !Bool,
    handshakeSent :: !Word64,
    handshakeRetries :: !Int
  }

-- | A reliable byte stream on a shared uTP socket. Reads and writes can run
-- concurrently; multiple readers/writers are serialized independently.
data UTP = UTP
  { endpoint :: UTPSocket,
    address :: SockAddr,
    sendId :: Word16,
    receiveId :: Word16,
    initialSyn :: Maybe (Word16, Word16),
    connection :: TVar Connection,
    readLock :: MVar (),
    writeLock :: MVar ()
  }

-- | The remote UDP address, including its port.
peerAddress :: UTP -> SockAddr
peerAddress = address

receiveCapacity, maxPayload, minPayload, maxPacketWindow, maxReceivePackets :: Int
receiveCapacity = 1024 * 1024
-- IPv6's minimum MTU, including IP, UDP, uTP and the largest generated SACK.
maxPayload = 1050
minPayload = 130
-- Less than half the sequence space, and representable by a 128-byte SACK.
maxPacketWindow = 1024
maxReceivePackets = 4096

microseconds :: IO Word64
microseconds = (`div` 1000) <$> getMonotonicTimeNSec

randomWord16 :: IO Word16
randomWord16 = BS.foldl' (\n byte -> n * 256 + fromIntegral byte) 0 <$> getEntropy 2

-- | Take ownership of a bound, unconnected UDP socket on success. Only this
-- manager may receive from it. IPv4 and IPv6 use the same implementation.
newUTPSocket :: Socket -> IO UTPSocket
newUTPSocket sock = mask $ \_ -> do
  -- A shared socket's kernel buffer must hold bursts from more than one peer.
  -- Preserve larger caller-supplied buffers; the OS may cap these requests.
  forM_ [NS.RecvBuffer, NS.SendBuffer] $ \option -> do
    size <- NS.getSocketOption sock option
    when (size < 4 * receiveCapacity) (NS.setSocketOption sock option (4 * receiveCapacity))
  routes <- newTVarIO Map.empty
  reserved <- newTVarIO Set.empty
  pending <- newTBQueueIO 128
  packets <- newTBQueueIO 4096
  failure <- newTVarIO Nothing
  ready <- newEmptyMVar
  let start action = forkIOWithUnmask $ \unmask -> do
        manager <- readMVar ready
        result <- try (unmask (action manager))
        case result of
          Left err -> failSocket manager (displayException (err :: SomeException))
          Right () -> pure ()
  reader <- start dispatchPackets
  sender <- start sendPackets `onException` killThread reader
  timer <- start maintainConnections `onException` (killThread reader >> killThread sender)
  let manager = UTPSocket sock routes reserved pending packets failure [reader, sender, timer]
  putMVar ready manager
  pure manager

checkSocket :: UTPSocket -> STM ()
checkSocket manager = readTVar (socketFailure manager) >>= maybe (pure ()) (throwSTM . userError)

failSocket :: UTPSocket -> String -> IO ()
failSocket manager message = do
  atomically $ do
    failure <- readTVar (socketFailure manager)
    when (failure == Nothing) $ do
      writeTVar (socketFailure manager) (Just message)
      streams <- readTVar (connections manager)
      forM_ streams $ \stream -> modifyTVar' (connection stream) (failed message)
      writeTVar (connections manager) Map.empty
      writeTVar (reservations manager) Set.empty
      void (flushTBQueue (accepting manager))
      void (flushTBQueue (outgoing manager))
  NS.close (socket manager)

-- | Close the UDP socket and fail all streams and blocked accept/connect calls.
-- Closing one 'UTP' instead never closes this manager or any other stream.
closeUTPSocket :: UTPSocket -> IO ()
closeUTPSocket manager = mask $ \_ -> do
  failSocket manager "uTP socket is closed"
  mapM_ killThread (workers manager)

newConnection :: Word64 -> Phase -> Word16 -> Word16 -> Word16 -> Word32 -> Connection
newConnection now state seqNr ackNr recvNr window = Connection
  { phase = state,
    nextSequence = seqNr,
    lastAck = ackNr,
    receivedSequence = recvNr,
    sentPackets = Seq.empty,
    sendBuffer = BS.empty,
    incoming = Seq.empty,
    bufferedBytes = 0,
    reordered = Map.empty,
    remoteFin = Nothing,
    remoteFinished = False,
    localFin = Nothing,
    remoteWindow = window,
    replyDelay = 0,
    delaySamples = [],
    recentDelays = [],
    queueDelay = 0,
    congestionWindow = fromIntegral (2 * maxPayload),
    lastReduction = 0,
    duplicateAcks = 0,
    roundTripTime = 0,
    roundTripVariance = 0,
    retransmissionTimeout = 1000000,
    lastReceived = now,
    lastActivity = now,
    lastProbe = 0,
    handshakeConfirmed = False,
    handshakeSent = now,
    handshakeRetries = 0
  }

newStream :: UTPSocket -> SockAddr -> Word16 -> Word16 -> Maybe (Word16, Word16) -> Connection -> IO UTP
newStream manager peer outgoingId incomingId syn state = do
  cell <- newTVarIO state
  reader <- newMVar ()
  writer <- newMVar ()
  pure (UTP manager peer outgoingId incomingId syn cell reader writer)

-- | Create a handle for connecting or accepting on a shared socket. This
-- handle is not a connected stream; use the stream returned by 'connectUTP'
-- or 'acceptUTP' for reads and writes.
newUTP :: UTPSocket -> IO UTP
newUTP manager = do
  now <- microseconds
  local <- NS.getSocketName (socket manager)
  newStream manager local 0 0 Nothing
    (newConnection now (Failed "uTP handle is not connected") 0 0 0 0)

register :: UTP -> STM Bool
register stream = do
  let manager = endpoint stream
      keys = Set.fromList [(address stream, sendId stream), (address stream, receiveId stream)]
  checkSocket manager
  routes <- readTVar (connections manager)
  reserved <- readTVar (reservations manager)
  if Map.size routes >= 1024 || not (Set.disjoint keys reserved)
    then pure False
    else do
      writeTVar (connections manager) (Map.insert (address stream, receiveId stream) stream routes)
      writeTVar (reservations manager) (Set.union keys reserved)
      pure True

unregister :: UTP -> STM ()
unregister stream = do
  let manager = endpoint stream
      key = (address stream, receiveId stream)
  routes <- readTVar (connections manager)
  -- A delayed cleanup must not remove a new connection reusing the same ID.
  case Map.lookup key routes of
    Just current | connection current == connection stream -> do
      writeTVar (connections manager) (Map.delete key routes)
      modifyTVar' (reservations manager) (Set.delete (address stream, sendId stream) . Set.delete key)
    _ -> pure ()

failed :: String -> Connection -> Connection
failed message state = state
  { phase = Failed message,
    sentPackets = Seq.empty,
    sendBuffer = BS.empty,
    incoming = Seq.empty,
    reordered = Map.empty,
    bufferedBytes = 0
  }

failStream :: UTP -> String -> STM ()
failStream stream message = do
  modifyTVar' (connection stream) (failed message)
  unregister stream

-- | Open a connection using the shared socket, including sockets that are also
-- accepting peers. IDs are reserved atomically; cancellation removes the route.
connectUTP :: UTP -> SockAddr -> IO UTP
connectUTP utp peer = mask $ \restore -> do
  now <- microseconds
  seed <- randomWord16
  let choose cid remaining = do
        when (remaining == (0 :: Int)) (fail "No free uTP connection IDs")
        let syn = SentPacket Syn BS.empty now now 1 False False
            state = (newConnection now Connecting 2 0 0 (fromIntegral receiveCapacity))
              { sentPackets = Seq.singleton (1, syn) }
        stream <- newStream (endpoint utp) peer (cid + 1) cid Nothing state
        registered <- atomically $ do
          count <- Map.size <$> readTVar (connections $ endpoint utp)
          when (count >= 1024) (throwSTM (userError "Too many uTP connections"))
          register stream
        if registered then pure stream else choose (cid + 1) (remaining - 1)
  stream <- choose seed 65536
  atomically (queuePacket stream Syn 1 BS.empty)
  restore (atomically $ do
    state <- readTVar (connection stream)
    case phase state of
      Connecting -> retry
      Connected -> pure stream
      Failed message -> throwSTM (userError message)
      _ -> throwSTM (userError "uTP connection closed during handshake"))
    `onException` abortUTP stream

-- | Wait for another incoming connection. This can be called repeatedly or
-- concurrently; accepted streams continue using this same UDP socket.
acceptUTP :: UTP -> IO UTP
acceptUTP utp = mask $ \_ -> atomically accept
  where
    accept = do
      checkSocket $ endpoint utp
      stream <- readTBQueue (accepting $ endpoint utp)
      state <- readTVar (connection stream)
      case phase state of
        Connected -> pure stream
        _ -> accept

-- Queue overflow drops a datagram, not a dispatcher thread. Reliability and
-- bounded retransmission recover from local drops just as from network drops.
enqueue :: UTPSocket -> SockAddr -> Packet -> Maybe UTP -> STM ()
enqueue manager peer packet owner = do
  full <- isFullTBQueue (outgoing manager)
  failure <- readTVar (socketFailure manager)
  unless (full || failure /= Nothing) (writeTBQueue (outgoing manager) (peer, packet, owner))

queuePacket :: UTP -> PacketType -> Word16 -> BS.ByteString -> STM ()
queuePacket stream kind seqNr body = do
  state <- readTVar (connection stream)
  let cid = if kind == Syn then receiveId stream else sendId stream
      exts = if kind == Data || kind == State || kind == Fin then selectiveAck state else []
      packet = Packet kind cid 0 (replyDelay state)
        (fromIntegral (receiveWindow state)) seqNr
        (receivedSequence state) exts body
  enqueue (endpoint stream) (address stream) packet (Just stream)

queueState :: UTP -> STM ()
queueState stream = do
  state <- readTVar (connection stream)
  queuePacket stream State (nextSequence state) BS.empty

receiveWindow :: Connection -> Int
receiveWindow state = max 0 (min (receiveCapacity - bufferedBytes state)
  ((maxReceivePackets - Seq.length (incoming state) - Map.size (reordered state)) * maxPayload))

selectiveAck :: Connection -> [(Word8, BS.ByteString)]
selectiveAck state
  | null offsets = []
  | otherwise = [(1, BS.pack [byte n | n <- [0 .. size - 1]])]
  where
    offsets = [distance (receivedSequence state + 2) seqNr | seqNr <- Map.keys (reordered state)]
    size = min 128 (4 * ((maximum offsets + 32) `div` 32))
    present = Set.fromList offsets
    byte n = foldl' (\value bit -> if Set.member (n * 8 + bit) present then setBit value bit else value) 0 [0 .. 7]

sendPackets :: UTPSocket -> IO ()
sendPackets manager = do
  (peer, packet, owner) <- atomically $ checkSocket manager >> readTBQueue (outgoing manager)
  live <- atomically $ case owner of
    Nothing -> pure True
    Just stream -> do
      state <- readTVar (connection stream)
      pure $ case phase state of
        Failed _ -> packetType packet == Reset
        TimeWait _ -> packetType packet == State || packetType packet == Reset
        _ -> True
  when live $ do
    now <- microseconds
    result <- tryIOError (NSB.sendTo (socket manager) (encodePacket packet { timestamp = fromIntegral now }) peer)
    case result of
      Left err -> forM_ owner $ \stream -> atomically (failStream stream (displayException err))
      Right _ -> pure ()
  sendPackets manager

dispatchPackets :: UTPSocket -> IO ()
dispatchPackets manager = do
  (raw, peer) <- NSB.recvFrom (socket manager) 65535
  case decodePacket raw of
    Left _ -> pure ()
    Right packet -> do
      now <- microseconds
      if packetType packet == Syn
        then receiveSyn manager peer packet now
        else atomically $ do
          routes <- readTVar (connections manager)
          case Map.lookup (peer, connectionId packet) routes of
            Just stream -> handlePacket stream packet now
            Nothing -> if packetType packet == Reset
              -- Unknown-connection RESETs echo the triggering packet's ID,
              -- which is our send ID rather than our normal receive ID.
              then forM_ [Map.lookup (peer, connectionId packet - 1) routes,
                          Map.lookup (peer, connectionId packet + 1) routes] $ \candidate ->
                forM_ candidate $ \stream -> when (sendId stream == connectionId packet) $
                  handlePacket stream packet now
              else queueReset manager peer packet
  dispatchPackets manager

queueReset :: UTPSocket -> SockAddr -> Packet -> STM ()
queueReset manager peer packet = enqueue manager peer
  (Packet Reset (connectionId packet) 0 0 0 0 (sequenceNumber packet) [] BS.empty) Nothing

receiveSyn :: UTPSocket -> SockAddr -> Packet -> Word64 -> IO ()
receiveSyn manager peer syn now = do
  existing <- atomically $ Map.lookup (peer, connectionId syn + 1) <$> readTVar (connections manager)
  case existing of
    Just stream -> atomically $ case initialSyn stream of
      Just (synSeq, stateSeq) | synSeq == sequenceNumber syn -> do
        state <- readTVar (connection stream)
        when (phase state == Connected || phase state == Closing) $
          queuePacket stream State stateSeq BS.empty
      _ -> pure ()
    Nothing -> do
      seqNr <- randomWord16
      let state = (newConnection now Connected (seqNr + 1) seqNr (sequenceNumber syn) (windowSize syn))
            { replyDelay = fromIntegral now - timestamp syn }
      stream <- newStream manager peer (connectionId syn) (connectionId syn + 1)
        (Just (sequenceNumber syn, seqNr)) state
      atomically $ do
        full <- isFullTBQueue (accepting manager)
        registered <- if full then pure False else register stream
        if registered
          then do
            writeTBQueue (accepting manager) stream
            queuePacket stream State seqNr BS.empty
          else do
            reserved <- readTVar (reservations manager)
            -- A colliding SYN must not RESET an existing adjacent-ID stream.
            unless (Set.member (peer, connectionId syn) reserved
              || Set.member (peer, connectionId syn + 1) reserved) (queueReset manager peer syn)

-- Word16 arithmetic is deliberately modular. All windows are smaller than
-- half the sequence space, so forward distances distinguish old and new data.
distance :: Word16 -> Word16 -> Int
distance a b = fromIntegral (b - a)

validAck :: Packet -> Connection -> Bool
validAck packet state = distance (lastAck state) (acknowledgementNumber packet)
  <= distance (lastAck state) (nextSequence state - 1)

handlePacket :: UTP -> Packet -> Word64 -> STM ()
handlePacket stream packet now = do
  state <- readTVar (connection stream)
  case phase state of
    Failed _ -> pure ()
    _ | packetType packet == Reset -> failStream stream "uTP connection reset"
    Connecting -> when (packetType packet == State && acknowledgementNumber packet == 1) $ do
      let established = state { phase = Connected, receivedSequence = sequenceNumber packet, handshakeConfirmed = True }
      writeTVar (connection stream) (acknowledge now packet (measureDelay now packet established))
      -- Confirm the passive handshake even when the application has no data.
      queueState stream
      pump stream now
    _ -> do
      let measured = measureDelay now packet state
          acked = if validAck packet state then acknowledge now packet measured else measured
          received = if packetType packet == Data || packetType packet == Fin
            then receiveSequenced packet acked else acked
          confirmed = received { handshakeConfirmed = handshakeConfirmed received || validAck packet state }
          finished = case (phase confirmed, localFin confirmed) of
            (Closing, Just fin) | lastAck confirmed == fin -> timeWait now confirmed
            _ -> confirmed
      writeTVar (connection stream) finished
      when (packetType packet == Data || packetType packet == Fin) (queueState stream)
      recoverFast stream now
      pump stream now

measureDelay :: Word64 -> Packet -> Connection -> Connection
measureDelay now packet state = state
  { replyDelay = fromIntegral now - timestamp packet,
    delaySamples = samples,
    recentDelays = recent,
    queueDelay = if delay == 0 then queueDelay state else max 0 (fromIntegral current),
    lastReceived = now,
    lastActivity = now
  }
  where
    delay = timestampDifference packet
    second = now `div` 1000000
    history = filter (\(time, _) -> second - time < 120) (delaySamples state)
    samples = if delay == 0 then history else case history of
      (time, value) : rest | time == second -> (time, minimumDelay value delay) : rest
      _ -> (second, delay) : history
    recent = if delay == 0 then recentDelays state else take 3 (delay : recentDelays state)
    baseline = foldr (minimumDelay . snd) delay samples
    current = fromIntegral (foldr minimumDelay delay recent - baseline) :: Int32

minimumDelay :: Word32 -> Word32 -> Word32
minimumDelay a b = if (fromIntegral (a - b) :: Int32) < 0 then a else b

acknowledge :: Word64 -> Packet -> Connection -> Connection
acknowledge now packet state = grown
  { lastAck = ackNr,
    sentPackets = remaining,
    remoteWindow = windowSize packet,
    duplicateAcks = if advance == 0 && windowSize packet == remoteWindow state
      && packetType packet == State && not (Seq.null remaining)
      then duplicateAcks state + 1 else 0
  }
  where
    ackNr = acknowledgementNumber packet
    advance = distance (lastAck state) ackNr
    (cumulative, rest) = Seq.spanl (\(seqNr, _) -> distance (lastAck state) seqNr <= advance) (sentPackets state)
    sack seqNr = any (\(kind, maskBytes) -> kind == 1 && offset >= 0
      && offset `div` 8 < BS.length maskBytes && testBit (BS.index maskBytes (offset `div` 8)) (offset `mod` 8)) (extensions packet)
      where offset = distance (ackNr + 2) seqNr
    newlySelective = Seq.filter (\(seqNr, sent) -> not (selectivelyAcked sent) && sack seqNr) rest
    remaining = fmap (\(seqNr, sent) -> (seqNr, sent { selectivelyAcked = selectivelyAcked sent || sack seqNr })) rest
    newlyAcked = filter (not . selectivelyAcked . snd) (toList cumulative) ++ toList newlySelective
    timed = foldl' (\s (_, sent) -> if transmissions sent == 1 then updateRTT now sent s else s) state newlyAcked
    ackedBytes = sum [BS.length (sentBody sent) | (_, sent) <- newlyAcked, sentType sent == Data]
    -- LEDBAT: at most one MTU of growth per window ACKed, with a 100ms target.
    -- Negative gain shrinks the window to zero; the timer then restarts with
    -- a minimum-size packet. Loss independently halves the window.
    gain = fromIntegral maxPayload * (1 - fromIntegral (queueDelay state) / 100000)
      * fromIntegral ackedBytes / max 1 (congestionWindow state)
    grown = timed { congestionWindow = max 0 (min (fromIntegral receiveCapacity) (congestionWindow state + gain)) }

updateRTT :: Word64 -> SentPacket -> Connection -> Connection
updateRTT now sent state = state
  { roundTripTime = rtt,
    roundTripVariance = variance,
    retransmissionTimeout = max 500000 (min 8000000 (rtt + 4 * variance))
  }
  where
    sample = fromIntegral (now - firstSent sent)
    first = roundTripTime state == 0
    rtt = if first then sample else roundTripTime state + (sample - roundTripTime state) `div` 8
    variance = if first then sample `div` 2 else roundTripVariance state
      + (abs (sample - roundTripTime state) - roundTripVariance state) `div` 4

receiveSequenced :: Packet -> Connection -> Connection
receiveSequenced packet state
  | remoteFinished state = state
  | offset == 0 || offset > maxPacketWindow = state
  | Map.member seqNr (reordered state) = state
  | maybe False (\fin -> offset > distance (receivedSequence state) fin) (remoteFin state) = state
  | bufferedBytes state + BS.length body > receiveCapacity = state
  | packetType packet == Data && slots >= maxReceivePackets = state
  -- Reserve space for the missing head packet, even if later packets fill
  -- the reorder queue. Otherwise selective ACKs can deadlock a full buffer.
  | offset > 1 && bufferedBytes state + BS.length body > receiveCapacity - 65535 = state
  | offset > 1 && slots >= maxReceivePackets - 1 = state
  | otherwise = drain inserted
  where
    seqNr = sequenceNumber packet
    body = payload packet
    offset = distance (receivedSequence state) seqNr
    slots = Seq.length (incoming state) + Map.size (reordered state)
    -- Remember an out-of-order FIN and never deliver bytes beyond it.
    kept = if packetType packet == Fin then Map.filterWithKey
      (\key _ -> distance (receivedSequence state) key <= offset) (reordered state) else reordered state
    dropped = sum [BS.length bytes | (_, bytes) <- Map.elems (reordered state)]
      - sum [BS.length bytes | (_, bytes) <- Map.elems kept]
    inserted = state
      { reordered = Map.insert seqNr (packetType packet, body) kept,
        bufferedBytes = bufferedBytes state + BS.length body - dropped,
        remoteFin = if packetType packet == Fin then Just seqNr else remoteFin state
      }
    drain current = case Map.lookup (receivedSequence current + 1) (reordered current) of
      Nothing -> current
      Just (kind, bytes) ->
        let next = receivedSequence current + 1
            closing = case phase current of TimeWait _ -> True; _ -> False
            consumed = current
              { receivedSequence = next,
                reordered = Map.delete next (reordered current),
                incoming = if kind == Data && not closing then incoming current |> bytes else incoming current,
                bufferedBytes = bufferedBytes current - if closing then BS.length bytes else 0,
                remoteFinished = kind == Fin
              }
         in if kind == Fin then consumed else drain consumed

inFlight :: Connection -> Int
inFlight state = sum [max 1 (BS.length (sentBody sent)) | (_, sent) <- toList (sentPackets state), not (selectivelyAcked sent)]

pump :: UTP -> Word64 -> STM ()
pump stream now = do
  state <- readTVar (connection stream)
  when (phase state == Connected || phase state == Closing) $ do
    let flight = inFlight state
        available = min (floor (congestionWindow state) - flight) (fromIntegral (remoteWindow state) - flight)
        probe = remoteWindow state == 0 && Seq.null (sentPackets state)
          && elapsed now (lastProbe state) >= retransmissionTimeout state
        size = if probe then 1 else min maxPayload available
        room = Seq.length (sentPackets state) < maxPacketWindow
    if not (BS.null (sendBuffer state)) && size > 0 && room
      then do
        let (body, rest) = BS.splitAt size (sendBuffer state)
        sendNew stream now Data body state
          { sendBuffer = rest, lastProbe = if probe then now else lastProbe state }
        pump stream now
      else when (phase state == Closing && BS.null (sendBuffer state) && localFin state == Nothing && room) $
        sendNew stream now Fin BS.empty state { localFin = Just (nextSequence state) }

sendNew :: UTP -> Word64 -> PacketType -> BS.ByteString -> Connection -> STM ()
sendNew stream now kind body state = do
  let seqNr = nextSequence state
      sent = SentPacket kind body now now 1 False False
  writeTVar (connection stream) state
    { nextSequence = seqNr + 1,
      sentPackets = sentPackets state |> (seqNr, sent),
      lastActivity = now
    }
  queuePacket stream kind seqNr body

recoverFast :: UTP -> Word64 -> STM ()
recoverFast stream now = do
  state <- readTVar (connection stream)
  let packets = toList (sentPackets state)
      reports = drop 1 (scanr (\(_, sent) count -> count + if selectivelyAcked sent then 1 else 0) (0 :: Int) packets)
      lost = [seqNr | ((seqNr, sent), count) <- zip packets reports,
        not (selectivelyAcked sent), not (fastRetransmitted sent), sentType sent /= Syn,
        count >= 3 || (duplicateAcks state >= 3 && Just seqNr == fmap fst (Seq.lookup 0 (sentPackets state)))]
  unless (null lost) $ do
    let reduced = if elapsed now (lastReduction state) >= max 1000 (roundTripTime state)
          then state { congestionWindow = congestionWindow state * 0.5, lastReduction = now } else state
        mark (seqNr, sent) = (seqNr, if seqNr `elem` lost
          then sent { transmissions = transmissions sent + 1, lastSent = now, fastRetransmitted = True } else sent)
    writeTVar (connection stream) reduced { sentPackets = fmap mark (sentPackets state), duplicateAcks = 0 }
    forM_ packets $ \(seqNr, sent) -> when (seqNr `elem` lost) $
      queuePacket stream (sentType sent) seqNr (sentBody sent)

elapsed :: Word64 -> Word64 -> Int
elapsed now before = fromIntegral (min (fromIntegral (maxBound :: Int)) (now - before))

timeWait :: Word64 -> Connection -> Connection
timeWait now state = state
  { phase = TimeWait (now + 30000000),
    incoming = Seq.empty,
    bufferedBytes = sum [BS.length bytes | (_, bytes) <- Map.elems (reordered state)],
    sendBuffer = BS.empty,
    sentPackets = Seq.empty
  }

maintainConnections :: UTPSocket -> IO ()
maintainConnections manager = do
  threadDelay 10000
  now <- microseconds
  streams <- atomically $ checkSocket manager >> Map.elems <$> readTVar (connections manager)
  forM_ streams $ \stream -> atomically $ do
    state <- readTVar (connection stream)
    case phase state of
      TimeWait expires | now >= expires -> unregister stream
      TimeWait _ -> pure ()
      Failed _ -> unregister stream
      _ -> do
        maintainHandshake stream now
        retransmit stream now
        current <- readTVar (connection stream)
        when (congestionWindow current < 1 && elapsed now (lastActivity current) >= retransmissionTimeout current) $
          modifyTVar' (connection stream) (\s -> s { congestionWindow = fromIntegral minPayload })
        pump stream now
  maintainConnections manager

maintainHandshake :: UTP -> Word64 -> STM ()
maintainHandshake stream now = do
  state <- readTVar (connection stream)
  case initialSyn stream of
    Just (_, seqNr) | not (handshakeConfirmed state)
      && elapsed now (handshakeSent state) >= min 8000000 (1000000 * 2 ^ min 3 (handshakeRetries state)) ->
        if handshakeRetries state >= 5
          then failStream stream "uTP handshake timed out"
          else do
            writeTVar (connection stream) state { handshakeSent = now, handshakeRetries = handshakeRetries state + 1 }
            queuePacket stream State seqNr BS.empty
    _ -> pure ()

retransmit :: UTP -> Word64 -> STM ()
retransmit stream now = do
  state <- readTVar (connection stream)
  case filter (not . selectivelyAcked . snd) (toList (sentPackets state)) of
    (seqNr, sent) : _ | elapsed now (lastSent sent) >= retransmissionTimeout state -> do
      let responsiveZeroWindow = remoteWindow state == 0 && elapsed now (lastReceived state) < 2 * retransmissionTimeout state
      if transmissions sent >= 6 && not responsiveZeroWindow
        then do
          queuePacket stream Reset (nextSequence state) BS.empty
          failStream stream "uTP acknowledgement timed out"
        else do
          let retrySent (key, value) = (key, if key == seqNr then value
                { lastSent = now, transmissions = min 6 (transmissions value + 1), fastRetransmitted = False } else value)
          writeTVar (connection stream) state
            { sentPackets = fmap retrySent (sentPackets state),
              congestionWindow = fromIntegral minPayload,
              retransmissionTimeout = min 8000000 (2 * retransmissionTimeout state),
              duplicateAcks = 0,
              lastReduction = now,
              lastActivity = now
            }
          queuePacket stream (sentType sent) seqNr (sentBody sent)
    _ -> pure ()

checkReadable :: Connection -> STM ()
checkReadable state = case phase state of
  Failed message -> throwSTM (userError message)
  Closing -> throwSTM (userError "uTP stream is closing")
  TimeWait _ -> throwSTM (userError "uTP stream is closed")
  _ -> pure ()

checkWritable :: Connection -> STM ()
checkWritable state = do
  checkReadable state
  when (phase state /= Connected) retry

takeIncoming :: Int -> Seq BS.ByteString -> (BS.ByteString, Seq BS.ByteString)
takeIncoming count chunks = case Seq.viewl chunks of
  EmptyL -> (BS.empty, chunks)
  bytes :< rest -> let (front, back) = BS.splitAt count bytes
    in (front, if BS.null back then rest else back Seq.<| rest)

instance PeerStream UTP where
  peekBytes stream n = withMVar (readLock stream) $ \() -> do
    when (n < 0) (fail "Negative peek length")
    if n == 0 then pure BS.empty else atomically $ do
      state <- readTVar (connection stream)
      checkReadable state
      let bytes = BS.take n (BS.concat (toList (incoming state)))
      when (BS.length bytes < n) $
        if remoteFinished state then throwSTM (userError "Unexpected EOF") else retry
      pure bytes

  readBytes stream n = withMVar (readLock stream) $ \() -> do
    when (n < 0) (fail "Negative read length")
    let go 0 chunks = pure (BS.concat (reverse chunks))
        go remaining chunks = do
          chunk <- atomically $ do
            state <- readTVar (connection stream)
            checkReadable state
            let (bytes, rest) = takeIncoming remaining (incoming state)
            when (BS.null bytes) $ if remoteFinished state then throwSTM (userError "Unexpected EOF") else retry
            writeTVar (connection stream) state { incoming = rest, bufferedBytes = bufferedBytes state - BS.length bytes }
            queueState stream
            pure bytes
          go (remaining - BS.length chunk) (chunk : chunks)
    go n []

  writeBytes stream bytes = withMVar (writeLock stream) $ \() -> mask $ \restore -> do
    let go rest = do
          atomically $ readTVar (connection stream) >>= checkWritable
          unless (BS.null rest) $ do
            let (batch, remaining) = BS.splitAt receiveCapacity rest
            atomically $ modifyTVar' (connection stream) (\s -> s { sendBuffer = batch })
            now <- microseconds
            atomically (pump stream now)
            restore (atomically $ do
              state <- readTVar (connection stream)
              checkWritable state
              unless (BS.null (sendBuffer state) && Seq.null (sentPackets state)) retry)
              `onException` abortUTP stream
            go remaining
    go bytes

-- | Abort only this connection, notifying the peer with RESET. The shared
-- socket and all other peers remain usable. Also safe after an operation fails.
abortUTP :: UTP -> IO ()
abortUTP stream = atomically $ do
  state <- readTVar (connection stream)
  case phase state of
    Failed _ -> unregister stream
    _ -> do
      queuePacket stream Reset (nextSequence state) BS.empty
      failStream stream "uTP stream is closed"

-- | Gracefully close only this stream. FIN is retransmitted until acknowledged;
-- a short TIME-WAIT route remains to ACK duplicate FINs, without a per-peer
-- thread. The shared socket is released only by 'closeUTPSocket'.
closeUTP :: UTP -> IO ()
closeUTP stream = withMVar (writeLock stream) $ \() -> mask $ \restore -> do
  now <- microseconds
  started <- atomically $ do
    state <- readTVar (connection stream)
    case phase state of
      Connected -> writeTVar (connection stream) state { phase = Closing }
      Connecting -> failStream stream "uTP stream closed during handshake"
      _ -> pure ()
    pump stream now
    pure (phase state == Connected || phase state == Closing)
  restore (atomically $ do
    state <- readTVar (connection stream)
    case phase state of
      TimeWait _ -> pure ()
      Failed message -> when started (throwSTM (userError message))
      _ -> retry) `onException` abortUTP stream
