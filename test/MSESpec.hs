{-# LANGUAGE OverloadedStrings #-}

module MSESpec (tests) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar
import Control.Exception (IOException, bracket, throwIO, try)
import Control.Monad (unless, void)
import qualified Crypto.Cipher.RC4 as RC4
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import Data.IORef
import Network.BitTorrent (AnyPeerStream (..), PeerStream (..))
import qualified Network.BitTorrent.Transport.MSE as MSE
import Network.BitTorrent.Transport.TCP (TCP (..), newTcpSocket)
import qualified Network.Socket as NS
import System.Timeout (timeout)
import TestSupport

tests :: [Test]
tests =
  [ ("MSE: RC4 library RFC 6229 drop-1024 vector", rc4Vector),
    ("MSE: bidirectional encrypted stream and fragmented reads", bounded roundTrip),
    ("MSE: legacy detection leaves the full handshake untouched", legacy),
    ("MSE: repeated TCP legacy detection preserves the handshake", bounded legacyTCP),
    ("MSE: truncated protocol prefix", rejects (MSE.respond lookupHash =<< memory "short")),
    ("MSE: invalid info hashes", invalidHashes),
    ("MSE: negative read rejected without changing state", bounded negativeRead)
  ]
    ++ [("MSE: initiator against reference, PadB/PadD=" ++ show n, bounded (initiatorReference n 2 n)) | n <- [0, 512]]
    ++ [("MSE: initiator ignores reserved selection bits", bounded (initiatorReference 0 0x82 0))]
    ++ [ ( "MSE: responder against reference, method/padding/IA=" ++ show (mode, n, size),
           bounded (responderReference mode n size)
         )
       | (mode, n, size) <- [(2, 0, 0), (2, 512, 65535), (1, 512, 68), (3, 0, 4), (0x82, 1, 1)]
       ]
    ++ [ ("MSE: initiator rejects selection " ++ show mode, bounded (rejects (initiatorReference 0 mode 0)))
       | mode <- [0, 1, 3, 4]
       ]
    ++ [ ("MSE: initiator rejects oversized PadD", bounded (rejects (initiatorReference 0 2 513))),
         ("MSE: initiator bounds PadB scan", bounded (rejects (initiatorReference 513 2 0)))
       ]
    ++ [ ("MSE: responder rejects " ++ label, bounded (malformedResponder label))
       | label <- ["unknown torrent", "bad VC", "unsupported method", "oversized PadC", "excess PadA", "truncated header"]
       ]
    ++ [ ( "MSE: rejects invalid DH public key " ++ show public,
           do
             rejects (MSE.respond lookupHash =<< memory (fixed 96 public))
             rejects (MSE.initiate infoHash =<< memory (fixed 96 public))
         )
       | public <- [0, 1, prime - 1, prime]
       ]

infoHash :: BS.ByteString
infoHash = SHA1.hash "test torrent"

lookupHash :: BS.ByteString -> IO (Maybe BS.ByteString)
lookupHash identifier = pure $ lookup identifier [(SHA1.hash ("req2" <> hash), hash) | hash <- [SHA1.hash "another torrent", infoHash]]

zeros :: Int -> BS.ByteString
zeros n = BS.replicate n 0

prime :: Integer
prime = 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A36210000000000090563

-- Independent reference peer: fixed 160-bit exponent, network-order codec,
-- DH calculation, and handshake parser, not the implementation's helpers.
private :: Integer
private = 0x8a17bc3491256789abcdef0123456789abcdef0123

power :: Integer -> Integer -> Integer
power _ 0 = 1
power base n =
  let half = power base (n `div` 2)
      squared = half * half `mod` prime
   in if odd n then squared * base `mod` prime else squared

number :: BS.ByteString -> Integer
number = BS.foldl' (\n byte -> n * 256 + fromIntegral byte) 0

fixed :: Int -> Integer -> BS.ByteString
fixed width n = BS.pack [fromIntegral (n `shiftR` (8 * i)) | i <- reverse [0 .. width - 1]]

referencePublic :: BS.ByteString
referencePublic = fixed 96 (power 2 private)

referenceSecret :: BS.ByteString -> BS.ByteString
referenceSecret public = fixed 96 (power (number public) private)

key :: BS.ByteString -> BS.ByteString -> RC4.State
key label secret = fst (RC4.combine (RC4.initialize (SHA1.hash (label <> secret <> infoHash))) (zeros 1024))

mix :: BS.ByteString -> BS.ByteString -> BS.ByteString
mix a b = BS.pack (BS.zipWith xor a b)

identity :: BS.ByteString -> BS.ByteString
identity secret = mix (SHA1.hash ("req2" <> infoHash)) (SHA1.hash ("req3" <> secret))

rc4Vector :: IO ()
rc4Vector = do
  let state = RC4.initialize (BS.pack [1, 2, 3, 4, 5])
      (dropped, _) = RC4.combine state (zeros 1024)
      (_, bytes) = RC4.combine dropped (zeros 16)
  assertEqual (BS.pack [0x30, 0xab, 0xbc, 0xc7, 0xc2, 0x0b, 0x01, 0x60, 0x9f, 0x23, 0xee, 0x2d, 0x5f, 0x6b, 0xb7, 0xdf]) bytes

bounded :: IO () -> IO ()
bounded action = timeout 5000000 action >>= maybe (fail "MSE test timed out") pure

rejects :: IO a -> IO ()
rejects action = do
  outcome <- try (void action) :: IO (Either IOException ())
  case outcome of
    Left _ -> pure ()
    Right () -> fail "Expected MSE rejection"

withWorker :: IO a -> (IO a -> IO b) -> IO b
withWorker action use = do
  result <- newEmptyMVar
  bracket (forkFinally action (putMVar result)) killThread $ \_ ->
    use (readMVar result >>= either throwIO pure)

withPair :: (TCP -> TCP -> IO a) -> IO a
withPair use =
  bracket
    (NS.socketPair NS.AF_UNIX NS.Stream NS.defaultProtocol)
    (\(a, b) -> NS.close a >> NS.close b)
    ( \(a, b) -> do
        left <- newTcpSocket a
        right <- newTcpSocket b
        use left right
    )

-- Deliberately return partial reads, including during legacy detection.
newtype Fragmented = Fragmented TCP

instance PeerStream Fragmented where
  peekBytes (Fragmented stream) = peekBytes stream
  readBytes (Fragmented stream) n = readBytes stream (min 3 n)
  writeBytes (Fragmented stream) = writeBytes stream

newtype Memory = Memory (IORef BS.ByteString)

memory :: BS.ByteString -> IO Memory
memory bytes = Memory <$> newIORef bytes

instance PeerStream Memory where
  peekBytes (Memory ref) n = do
    bytes <- readIORef ref
    if n < 0 || BS.length bytes < n
      then fail "Invalid peek length or unexpected EOF"
      else pure (BS.take n bytes)
  readBytes (Memory ref) n = atomicModifyIORef' ref $ \bytes ->
    let (prefix, rest) = BS.splitAt (min 3 n) bytes in (rest, prefix)
  writeBytes _ _ = pure ()

legacy :: IO ()
legacy = do
  let handshake = BS.cons 19 "BitTorrent protocol" <> zeros 8 <> infoHash <> BS.replicate 20 1
  stream <- memory (handshake <> "untouched")
  result <- MSE.respond lookupHash stream
  case result of
    Nothing -> pure ()
    Just _ -> fail "Legacy handshake detected as MSE"
  chunks <- mapM (readBytes stream) (replicate 22 3 ++ [2])
  assertEqual handshake (BS.concat chunks)
  assertEqual "unt" =<< readBytes stream 3

legacyTCP :: IO ()
legacyTCP = withPair $ \a b -> do
  let handshake = BS.cons 19 "BitTorrent protocol" <> zeros 8 <> infoHash <> BS.replicate 20 1
      stream = AnyPeerStream b
  writeBytes a handshake
  assertEqual "" =<< peekBytes stream 0
  rejects (peekBytes stream (-1))
  mapM_
    ( \_ -> do
        result <- MSE.respond lookupHash stream
        case result of
          Nothing -> pure ()
          Just _ -> fail "Legacy handshake detected as MSE"
    )
    [1, 2 :: Int]
  assertEqual handshake =<< peekBytes stream 68
  assertEqual handshake =<< readBytes stream 68

invalidHashes :: IO ()
invalidHashes =
  mapM_
    ( \n -> do
        stream <- memory ""
        rejects (MSE.initiate (zeros n) stream)
    )
    [0, 19, 21]

accept :: (PeerStream a) => a -> IO (MSE.MSE a)
accept stream = MSE.respond lookupHash stream >>= maybe (fail "Expected MSE") (\(hash, mse) -> assertEqual infoHash hash >> pure mse)

roundTrip :: IO ()
roundTrip = withPair $ \a b -> withWorker (accept (Fragmented b)) $ \accepted -> do
  sender <- MSE.initiate infoHash (Fragmented a)
  receiver <- accepted
  let payload = BS.concat (replicate 32 (BS.pack [0 .. 255]))
  withWorker (readBytes sender 4) $ \reply -> do
    writeBytes sender ""
    assertEqual "" =<< readBytes sender 0
    writeBytes sender (BS.take 17 payload)
    writeBytes sender (BS.drop 17 payload)
    assertEqual (BS.take 31 payload) =<< peekBytes receiver 31
    assertEqual (BS.take 17 payload) =<< peekBytes receiver 17
    first <- readBytes receiver 31
    rest <- readBytes receiver (BS.length payload - 31)
    assertEqual payload (first <> rest)
    writeBytes receiver "pong"
    assertEqual "pong" =<< reply

negativeRead :: IO ()
negativeRead = withPair $ \a b -> withWorker (accept b) $ \accepted -> do
  sender <- MSE.initiate infoHash a
  receiver <- accepted
  rejects (readBytes receiver (-1))
  rejects (peekBytes receiver (-1))
  writeBytes sender "ok"
  assertEqual "ok" =<< readBytes receiver 2

-- Reference synchronization consumes exactly through the marker.
scan :: TCP -> BS.ByteString -> IO ()
scan stream marker = loop BS.empty
  where
    loop bytes
      | marker `BS.isSuffixOf` bytes = pure ()
      | BS.length bytes == 512 + BS.length marker = fail "Reference sync failed"
      | otherwise = readBytes stream 1 >>= loop . (bytes <>)

decrypt :: TCP -> RC4.State -> Int -> IO (RC4.State, BS.ByteString)
decrypt stream state n = RC4.combine state <$> readBytes stream n

-- Acts as B, checking the complete A->B handshake, then sends a scripted
-- response with controllable selection and padding lengths.
initiatorReference :: Int -> Integer -> Int -> IO ()
initiatorReference padB selection padD = withPair $ \a b ->
  withWorker (reference b) $ \finished -> do
    sender <- MSE.initiate infoHash (Fragmented a)
    assertEqual "reference payload" =<< readBytes sender 17
    writeBytes sender "client payload"
    finished
  where
    reference stream = do
      public <- readBytes stream 96
      let secret = referenceSecret public
      writeBytes stream (referencePublic <> BS.replicate padB 0x57)
      scan stream (SHA1.hash ("req1" <> secret))
      assertEqual (identity secret) =<< readBytes stream 20
      (headerState, header) <- decrypt stream (key "keyA" secret) 14
      assertEqual (zeros 8 <> fixed 4 2) (BS.take 12 header)
      let size = fromIntegral (number (BS.drop 12 header))
      unless (size <= 512) (fail "Oversized outgoing PadC")
      (paddingState, padding) <- decrypt stream headerState size
      assertEqual (zeros size) padding
      (received, initialLength) <- decrypt stream paddingState 2
      assertEqual (zeros 2) initialLength
      let response = zeros 8 <> fixed 4 selection <> fixed 2 (fromIntegral padD) <> zeros padD
          (sent, encoded) = RC4.combine (key "keyB" secret) response
          (_, payload) = RC4.combine sent ("reference payload" :: BS.ByteString)
      writeBytes stream (encoded <> payload)
      (_, message) <- decrypt stream received 14
      assertEqual "client payload" message

-- Acts as A, including encrypted IA even when plaintext payload is selected.
responderReference :: Integer -> Int -> Int -> IO ()
responderReference offered padding initialSize = withPair $ \a b ->
  withWorker (accept (Fragmented b)) $ \accepted -> do
    writeBytes a (referencePublic <> BS.replicate padding 0xa5)
    public <- readBytes a 96
    let secret = referenceSecret public
        initial = BS.take initialSize (BS.concat (replicate 256 (BS.pack [0 .. 255])))
        selected = if offered .&. 2 /= 0 then 2 else 1
        request =
          zeros 8
            <> fixed 4 offered
            <> fixed 2 (fromIntegral padding)
            <> zeros padding
            <> fixed 2 (fromIntegral initialSize)
            <> initial
        (sent, encoded) = RC4.combine (key "keyA" secret) request
    writeBytes a (SHA1.hash ("req1" <> secret) <> identity secret <> encoded)
    let (afterVC, marker) = RC4.combine (key "keyB" secret) (zeros 8)
    scan a marker
    (headerState, header) <- decrypt a afterVC 6
    assertEqual selected (number (BS.take 4 header))
    let size = fromIntegral (number (BS.drop 4 header))
    unless (size <= 512) (fail "Oversized outgoing PadD")
    (received, padD) <- decrypt a headerState size
    assertEqual (zeros size) padD
    receiver <- accepted
    let (_, encryptedPayload) = RC4.combine sent ("next payload" :: BS.ByteString)
    writeBytes a (if selected == 2 then encryptedPayload else "next payload")
    assertEqual (BS.take 12 (initial <> "next payload")) =<< peekBytes receiver 12
    first <- readBytes receiver (min 13 initialSize)
    rest <- readBytes receiver (initialSize - BS.length first + 12)
    assertEqual (initial <> "next payload") (first <> rest)
    writeBytes receiver "reply"
    reply <- readBytes a 5
    assertEqual "reply" (if selected == 2 then snd (RC4.combine received reply) else reply)

malformedResponder :: String -> IO ()
malformedResponder label = withPair $ \a b -> withWorker (accept b) $ \accepted -> do
  writeBytes a referencePublic
  public <- readBytes a 96
  let secret = referenceSecret public
      torrent = if label == "unknown torrent" then zeros 20 else identity secret
      verification = if label == "bad VC" then BS.replicate 8 1 else zeros 8
      offered = if label == "unsupported method" then 0x80 else 2
      size = if label == "oversized PadC" then 513 else 0
      header = verification <> fixed 4 offered <> fixed 2 size <> zeros 2
      (_, encoded) = RC4.combine (key "keyA" secret) header
      padA = if label == "excess PadA" then BS.replicate 513 0x55 else ""
      body = SHA1.hash ("req1" <> secret) <> torrent <> encoded
  writeBytes a (padA <> if label == "truncated header" then BS.take 45 body else body)
  NS.shutdown (inner a) NS.ShutdownSend
  rejects accepted
