module Network.BitTorrent.Transport.UTP
  ( UTP,
    connectUTP,
    acceptUTP,
    closeUTP,
    PacketType (..),
    Packet (..),
    encodePacket,
    decodePacket,
  ) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, displayException, finally, mask, onException, try)
import Control.Monad (unless, void, when)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, toLazyByteString, word8, word16BE, word32BE)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.BitTorrent (PeerStream (..))
import Network.Socket (SockAddr, Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Entropy (getEntropy)
import System.Timeout (timeout)

data PacketType = Data | Fin | State | Reset | Syn
  deriving (Eq, Show, Enum, Bounded)

-- | BEP 29 version 1. Extension bodies exclude their next/length bytes.
data Packet = Packet
  { packetType :: PacketType,
    connectionId :: Word16,
    timestamp :: Word32,
    timestampDifference :: Word32,
    windowSize :: Word32,
    sequenceNumber :: Word16,
    acknowledgementNumber :: Word16,
    extensions :: [(Word8, BS.ByteString)],
    payload :: BS.ByteString
  }
  deriving (Eq, Show)

encodePacket :: Packet -> BS.ByteString
encodePacket packet
  | any invalid (extensions packet) = error "Invalid uTP extension"
  | otherwise = BL.toStrict . toLazyByteString $
      word8 (fromIntegral (fromEnum (packetType packet)) `shiftL` 4 .|. 1)
        <> word8 (firstExtension (extensions packet))
        <> word16BE (connectionId packet)
        <> word32BE (timestamp packet)
        <> word32BE (timestampDifference packet)
        <> word32BE (windowSize packet)
        <> word16BE (sequenceNumber packet)
        <> word16BE (acknowledgementNumber packet)
        <> encodeExtensions (extensions packet)
        <> byteString (payload packet)
  where
    invalid (kind, body) = kind == 0 || BS.length body > 255
    firstExtension [] = 0
    firstExtension ((kind, _) : _) = kind
    encodeExtensions [] = mempty
    encodeExtensions ((_, body) : rest) =
      word8 (firstExtension rest)
        <> word8 (fromIntegral (BS.length body))
        <> byteString body
        <> encodeExtensions rest

decodePacket :: BS.ByteString -> Either String Packet
decodePacket raw
  | BS.length raw < 20 = Left "Truncated uTP header"
  | BS.head raw .&. 15 /= 1 = Left "Unsupported uTP version"
  | kind > 4 = Left "Invalid uTP packet type"
  | otherwise = do
      (exts, body) <- decodeExtensions (BS.index raw 1) (BS.drop 20 raw)
      when (kind == 0 && BS.null body) $
        Left "Empty uTP data payload"
      when (kind /= 0 && not (BS.null body)) $
        Left "Unexpected uTP control payload"
      pure Packet
        { packetType = toEnum (fromIntegral kind),
          connectionId = word16 2,
          timestamp = word32 4,
          timestampDifference = word32 8,
          windowSize = word32 12,
          sequenceNumber = word16 16,
          acknowledgementNumber = word16 18,
          extensions = exts,
          payload = body
        }
  where
    kind = BS.head raw `shiftR` 4
    word16 offset = fromIntegral (BS.index raw offset) `shiftL` 8 .|. fromIntegral (BS.index raw (offset + 1))
    word32 offset = BS.foldl' (\n byte -> n `shiftL` 8 .|. fromIntegral byte) 0 (BS.take 4 (BS.drop offset raw))
    decodeExtensions 0 body = Right ([], body)
    decodeExtensions ext body
      | BS.length body < 2 = Left "Truncated uTP extension header"
      | BS.length body < 2 + size = Left "Truncated uTP extension body"
      | ext == 1 && (size == 0 || size `mod` 4 /= 0) = Left "Invalid uTP selective acknowledgement"
      | otherwise = do
          (rest, bytes) <- decodeExtensions (BS.head body) (BS.drop (2 + size) body)
          pure ((ext, BS.take size (BS.drop 2 body)) : rest, bytes)
      where
        size = fromIntegral (BS.index body 1)

data Connection = Connection
  { nextSequence :: Word16,
    receivedSequence :: Word16,
    pendingSequence :: Maybe Word16,
    incoming :: BS.ByteString,
    remoteWindow :: Word32,
    replyDelay :: Word32,
    delaySamples :: [(Word64, Word32)],
    pacingDelay :: Int,
    roundTripTime :: Int,
    roundTripVariance :: Int,
    retransmissionTimeout :: Int,
    remoteFinished :: Bool,
    localFinished :: Bool,
    failure :: Maybe String
  }

-- | A conservative, single-packet-window uTP stream. Reads and writes may run
-- concurrently; multiple readers/writers are serialized independently. Receive
-- storage is bounded. Out-of-order packets are cumulatively acknowledged and
-- retransmitted, rather than buffered or selectively acknowledged.
data UTP = UTP
  { socket :: Socket,
    address :: SockAddr,
    sendId :: Word16,
    receiveId :: Word16,
    initialSyn :: Maybe (Word16, Word16),
    connection :: MVar Connection,
    readLock :: MVar (),
    writeLock :: MVar (),
    readable :: MVar (),
    acknowledged :: MVar (),
    receiver :: ThreadId,
    closed :: MVar Bool
  }

receiveCapacity :: Int
receiveCapacity = 1024 * 1024

-- Keep datagrams below the IPv6 minimum MTU, including UDP/IP headers.
maxPayload :: Int
maxPayload = 1180

microseconds :: IO Word64
microseconds = (`div` 1000) <$> getMonotonicTimeNSec

randomWord16 :: IO Word16
randomWord16 = BS.foldl' (\n byte -> n `shiftL` 8 .|. fromIntegral byte) 0 <$> getEntropy 2

-- | Connect using an unconnected UDP socket. On success the stream owns the
-- socket; on failure the caller still owns it. No other code may receive from
-- this socket. IPv4 and IPv6 addresses are supported by the socket supplied.
connectUTP :: Socket -> SockAddr -> IO UTP
connectUTP sock peer = mask $ \restore -> do
  cid <- randomWord16
  let seqNr = 1
      syn = Packet Syn cid 0 0 (fromIntegral receiveCapacity) seqNr 0 [] BS.empty
      receiveState = do
        (raw, source) <- NSB.recvFrom sock 65535
        case decodePacket raw of
          Right packet
            | source == peer && connectionId packet == cid ->
                case packetType packet of
                  Reset -> fail "uTP connection reset"
                  State | acknowledgementNumber packet == seqNr -> pure packet
                  _ -> receiveState
          _ -> receiveState
      attempt retries = do
        now <- microseconds
        void (NSB.sendTo sock (encodePacket syn { timestamp = fromIntegral now }) peer)
        result <- timeout (retryDelay 1000000 retries) receiveState
        case result of
          Just packet -> pure packet
          Nothing | retries < 5 -> attempt (retries + 1)
          Nothing -> fail "uTP connection timed out"
  state <- restore (attempt 0)
  startUTP sock peer (cid + 1) cid Nothing (seqNr + 1) (sequenceNumber state) state

-- | Accept one connection on a bound UDP socket, transferring ownership on
-- success. This is not a multiplexing listener: use one socket per connection.
acceptUTP :: Socket -> IO UTP
acceptUTP sock = mask $ \restore -> do
  let accept = do
        (raw, peer) <- restore (NSB.recvFrom sock 65535)
        case decodePacket raw of
          Right syn | packetType syn == Syn -> do
            seqNr <- randomWord16
            stream <- startUTP sock peer (connectionId syn) (connectionId syn + 1)
              (Just (sequenceNumber syn, seqNr)) (seqNr + 1) (sequenceNumber syn) syn
            -- The initial STATE, unlike subsequent ACKs, consumes a sequence number.
            restore (sendPacket stream State seqNr BS.empty) `onException` stopUTP stream
            pure stream
          _ -> accept
  accept

startUTP :: Socket -> SockAddr -> Word16 -> Word16 -> Maybe (Word16, Word16) -> Word16 -> Word16 -> Packet -> IO UTP
startUTP sock peer outgoingId incomingId syn seqNr ackNr packet = mask $ \_ -> do
  now <- microseconds
  state <- newMVar Connection
    { nextSequence = seqNr,
      receivedSequence = ackNr,
      pendingSequence = Nothing,
      incoming = BS.empty,
      remoteWindow = windowSize packet,
      replyDelay = fromIntegral now - timestamp packet,
      delaySamples = [],
      pacingDelay = 0,
      roundTripTime = 0,
      roundTripVariance = 0,
      retransmissionTimeout = 1000000,
      remoteFinished = False,
      localFinished = False,
      failure = Nothing
    }
  readerLock <- newMVar ()
  writerLock <- newMVar ()
  readEvent <- newEmptyMVar
  ackEvent <- newEmptyMVar
  isClosed <- newMVar False
  ready <- newEmptyMVar
  tid <- forkIOWithUnmask $ \unmask -> do
    result <- try (unmask (readMVar ready >>= receivePackets))
    case result of
      Left err -> modifyMVar_ state $ \s -> pure s { failure = Just (displayException (err :: SomeException)) }
      Right () -> pure ()
    void (tryPutMVar readEvent ())
    void (tryPutMVar ackEvent ())
  let stream = UTP sock peer outgoingId incomingId syn state readerLock writerLock readEvent ackEvent tid isClosed
  putMVar ready stream
  pure stream

sendPacket :: UTP -> PacketType -> Word16 -> BS.ByteString -> IO ()
sendPacket stream kind seqNr body = do
  state <- readMVar (connection stream)
  now <- microseconds
  let packet = Packet kind (sendId stream) (fromIntegral now) (replyDelay state)
        (fromIntegral (receiveCapacity - BS.length (incoming state))) seqNr
        (receivedSequence state) [] body
  void (NSB.sendTo (socket stream) (encodePacket packet) (address stream))

sendState :: UTP -> IO ()
sendState stream = do
  state <- readMVar (connection stream)
  sendPacket stream State (nextSequence state) BS.empty

receivePackets :: UTP -> IO ()
receivePackets stream = do
  (raw, source) <- NSB.recvFrom (socket stream) 65535
  case decodePacket raw of
    Right packet | source == address stream -> do
      if packetType packet == Syn
        then case initialSyn stream of
          Just (synSeq, stateSeq)
            | connectionId packet == sendId stream && sequenceNumber packet == synSeq ->
                sendPacket stream State stateSeq BS.empty
          _ -> pure ()
        else when (connectionId packet == receiveId stream) $ do
          now <- microseconds
          modifyMVar_ (connection stream) $ \state -> do
            let expected = sequenceNumber packet == receivedSequence state + 1
                hasRoom = BS.length (incoming state) + BS.length (payload packet) <= receiveCapacity
                accept = expected && hasRoom && not (remoteFinished state)
                sequenced = packetType packet == Data || packetType packet == Fin
                delay = timestampDifference packet
                -- One minimum per second bounds storage and ages out clock
                -- drift/route changes after two minutes.
                second = now `div` 1000000
                recent = filter (\(time, _) -> second - time < 120) (delaySamples state)
                samples = if delay == 0 then recent else case recent of
                  (time, value) : rest | time == second -> (time, minimumDelay value delay) : rest
                  _ -> (second, delay) : recent
                baseline = foldr (minimumDelay . snd) delay samples
                queueDelay = max 0 (fromIntegral (delay - baseline) :: Int32)
                -- A one-packet congestion window with additional LEDBAT-style
                -- pacing above the 100ms target. No burst can exceed one MTU.
                pace = fromIntegral (min 1000000 (max 0 (queueDelay - 100000)))
            pure state
              { remoteWindow = windowSize packet,
                replyDelay = fromIntegral now - timestamp packet,
                delaySamples = samples,
                pacingDelay = pace,
                pendingSequence = if pendingSequence state == Just (acknowledgementNumber packet)
                  then Nothing else pendingSequence state,
                receivedSequence = if sequenced && accept then sequenceNumber packet else receivedSequence state,
                incoming = if packetType packet == Data && accept
                  then incoming state <> payload packet else incoming state,
                remoteFinished = remoteFinished state || (packetType packet == Fin && accept),
                failure = if packetType packet == Reset then Just "uTP connection reset" else failure state
              }
          void (tryPutMVar (readable stream) ())
          void (tryPutMVar (acknowledged stream) ())
          when (packetType packet == Data || packetType packet == Fin) (sendState stream)
    _ -> pure ()
  receivePackets stream

checkConnection :: Connection -> IO ()
checkConnection state = maybe (pure ()) fail (failure state)

-- Timestamp differences wrap every ~72 minutes, just like packet timestamps.
minimumDelay :: Word32 -> Word32 -> Word32
minimumDelay a b = if (fromIntegral (a - b) :: Int32) < 0 then a else b

retryDelay :: Int -> Int -> Int
retryDelay base retries = min 8000000 (base * 2 ^ min retries 3)

-- After the handshake, STATE packets do not consume sequence numbers;
-- DATA and FIN do.
sendReliable :: UTP -> PacketType -> BS.ByteString -> IO ()
sendReliable stream kind body = mask $ \restore -> do
  seqNr <- modifyMVar (connection stream) $ \state -> do
    checkConnection state
    when (localFinished state) (fail "uTP stream is closed")
    pure (state { nextSequence = nextSequence state + 1, pendingSequence = Just (nextSequence state) }, nextSequence state)
  let waitAck = do
        state <- readMVar (connection stream)
        checkConnection state
        unless (pendingSequence state == Nothing) $ takeMVar (acknowledged stream) >> waitAck
      attempt retries = do
        started <- microseconds
        state <- readMVar (connection stream)
        sendPacket stream kind seqNr body
        result <- timeout (retryDelay (retransmissionTimeout state) retries) waitAck
        case result of
          Just () -> when (retries == 0) $ do
            finished <- microseconds
            let sample = fromIntegral (finished - started)
            -- Karn's rule: retransmitted packets cannot give an unambiguous
            -- RTT sample. Use BEP 29's smoothed RTT/variance otherwise.
            modifyMVar_ (connection stream) $ \current -> do
              let first = roundTripTime current == 0
                  rtt = if first then sample else roundTripTime current + (sample - roundTripTime current) `div` 8
                  variance = if first then sample `div` 2 else roundTripVariance current
                    + (abs (sample - roundTripTime current) - roundTripVariance current) `div` 4
              pure current
                { roundTripTime = rtt,
                  roundTripVariance = variance,
                  retransmissionTimeout = max 500000 (min 8000000 (rtt + 4 * variance))
                }
          Nothing | retries < 5 -> attempt (retries + 1)
          Nothing -> fail "uTP acknowledgement timed out"
  restore (attempt 0) `onException` do
    modifyMVar_ (connection stream) $ \state ->
      pure state { failure = Just "uTP write interrupted or timed out" }
    void (tryPutMVar (readable stream) ())
    void (tryPutMVar (acknowledged stream) ())

instance PeerStream UTP where
  readBytes stream n = withMVar (readLock stream) $ \() -> do
    when (n < 0) (fail "Negative read length")
    let go 0 chunks = pure (BS.concat (reverse chunks))
        go remaining chunks = do
          chunk <- modifyMVar (connection stream) $ \state -> do
            checkConnection state
            let (bytes, rest) = BS.splitAt remaining (incoming state)
            when (BS.null bytes && remoteFinished state) (fail "Unexpected EOF")
            pure (state { incoming = rest }, bytes)
          if BS.null chunk
            then takeMVar (readable stream) >> go remaining chunks
            else do
              sendState stream
              go (remaining - BS.length chunk) (chunk : chunks)
    go n []

  writeBytes stream bytes = withMVar (writeLock stream) $ \() -> do
    let go rest = do
          state <- readMVar (connection stream)
          checkConnection state
          when (localFinished state) (fail "uTP stream is closed")
          unless (BS.null rest) $ do
            let size = min maxPayload (fromIntegral (min (fromIntegral maxPayload) (remoteWindow state)))
            if size == 0
              -- A retransmitted one-byte DATA packet probes a zero window. It
              -- is only acknowledged once receive storage becomes available.
              then sendReliable stream Data (BS.take 1 rest) >> go (BS.drop 1 rest)
              else do
                threadDelay (pacingDelay state)
                sendReliable stream Data (BS.take size rest)
                go (BS.drop size rest)
    go bytes

stopUTP :: UTP -> IO ()
stopUTP stream = do
  killThread (receiver stream)
  modifyMVar_ (connection stream) $ \state -> pure state { failure = Just "uTP stream is closed" }
  void (tryPutMVar (readable stream) ())
  void (tryPutMVar (acknowledged stream) ())

abortUTP :: UTP -> IO ()
abortUTP stream = stopUTP stream `finally` NS.close (socket stream)

-- | Send and await FIN, then release the socket even if the peer is unreachable.
-- Repeated closes are harmless. Buffered peer data may be read before EOF while
-- the connection remains open; closing locally releases the entire stream.
closeUTP :: UTP -> IO ()
closeUTP stream = mask $ \restore -> do
  first <- modifyMVar (closed stream) $ \value -> pure (True, not value)
  when first $ restore (withMVar (writeLock stream) $ \() -> do
    sendReliable stream Fin BS.empty
    modifyMVar_ (connection stream) $ \state -> pure state { localFinished = True })
    `finally` abortUTP stream
