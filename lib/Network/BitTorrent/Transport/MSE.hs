{-# LANGUAGE OverloadedStrings #-}

module Network.BitTorrent.Transport.MSE (MSE, initiate, respond) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad (unless, when)
import qualified Crypto.Cipher.RC4 as RC4
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits (Bits (setBit, shiftL, shiftR, xor, (.&.), (.|.)))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.BitTorrent (PeerStream (readBytes, writeBytes))
import System.Entropy (getEntropy)
import Prelude hiding (exponent)

btPrefix :: BS.ByteString
btPrefix = BS.cons 19 "BitTorrent protocol"

data IncomingProtocol
  = IncomingPlaintext
  | IncomingMSE
  deriving (Eq, Show)

detectProtocol ::
  (PeerStream a) =>
  a ->
  IO
    (IncomingProtocol, BS.ByteString)
detectProtocol stream = do
  prefix <- readExactly stream 20

  let protocol =
        if prefix == btPrefix
          then IncomingPlaintext
          else IncomingMSE

  pure (protocol, prefix)

dhKeySize :: Int
dhKeySize = 96

dhPrime :: Integer
dhPrime = 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A36210000000000090563

integerToFixedBytes :: Int -> Integer -> Either String ByteString
integerToFixedBytes width n
  | width < 0 = Left "negative width"
  | n < 0 = Left "negative integer"
  | n >= 256 ^ width = Left "integer does not fit"
  | otherwise =
      Right $
        BS.pack
          [ fromIntegral ((n `shiftR` (8 * i)) .&. 0xff)
          | i <- reverse [0 .. width - 1]
          ]

bytesToInteger :: ByteString -> Integer
bytesToInteger =
  BS.foldl' (\acc byte -> (acc `shiftL` 8) .|. fromIntegral byte) 0

generatePrivateExponent :: IO Integer
generatePrivateExponent = (`setBit` 159) . bytesToInteger <$> getEntropy 20

generatePublicKey :: Integer -> Integer -> Either String ByteString
generatePublicKey prime private =
  powMod 2 private prime
    >>= integerToFixedBytes dhKeySize

powMod :: Integer -> Integer -> Integer -> Either String Integer
powMod base exponent modulus
  | modulus <= 0 = Left "invalid modulus"
  | exponent < 0 = Left "negative exponent"
  | otherwise = Right $ go (base `mod` modulus) exponent 1
  where
    go _ 0 acc = acc `mod` modulus
    go b e acc
      | odd e =
          go
            ((b * b) `mod` modulus)
            (e `div` 2)
            ((acc * b) `mod` modulus)
      | otherwise =
          go
            ((b * b) `mod` modulus)
            (e `div` 2)
            acc

deriveSharedSecret :: Integer -> Integer -> ByteString -> Either String ByteString
deriveSharedSecret prime private remotePublic
  | BS.length remotePublic /= dhKeySize =
      Left "invalid public key length"
  | public < 2 || public > prime - 2 =
      Left "invalid public key"
  | otherwise =
      powMod public private prime
        >>= integerToFixedBytes dhKeySize
  where
    public = bytesToInteger remotePublic

data MSE a = MSE a (MVar (Maybe RC4.State, ByteString)) (MVar (Maybe RC4.State))

maxPadding :: Int
maxPadding = 512

vc :: ByteString
vc = BS.replicate 8 0

check :: Bool -> String -> IO ()
check condition message = unless condition (fail ("MSE: " ++ message))

fromEither :: Either String a -> IO a
fromEither = either (fail . ("MSE: " ++)) pure

readExactly :: (PeerStream a) => a -> Int -> IO ByteString
readExactly stream count = do
  check (count >= 0) "negative read length"
  let go 0 chunks = pure (BS.concat (reverse chunks))
      go remaining chunks = do
        chunk <- readBytes stream remaining
        check (not (BS.null chunk)) "unexpected EOF"
        check (BS.length chunk <= remaining) "transport returned too many bytes"
        go (remaining - BS.length chunk) (chunk : chunks)
  go count []

randomPadding :: IO ByteString
randomPadding =
  bytesToInteger <$> getEntropy 2 >>= \size ->
    getEntropy (fromIntegral (size `mod` 513))

zeroPadding :: IO ByteString
zeroPadding = BS.map (const 0) <$> randomPadding

word16 :: Int -> ByteString
word16 n = BS.pack [fromIntegral (n `shiftR` 8), fromIntegral n]

method :: Int -> ByteString
method n = BS.pack [0, 0, 0, fromIntegral n]

xorBytes :: ByteString -> ByteString -> ByteString
xorBytes a b = BS.pack (BS.zipWith xor a b)

cipher :: ByteString -> ByteString -> ByteString -> RC4.State
cipher label secret skey =
  fst (RC4.combine (RC4.initialize (SHA1.hash (label <> secret <> skey))) (BS.replicate 1024 0))

synchronize :: (PeerStream a) => a -> ByteString -> IO ()
synchronize stream marker = do
  first <- readExactly stream (BS.length marker)
  let go skipped window
        | window == marker = pure ()
        | skipped == maxPadding = fail "MSE: padding synchronization failed"
        | otherwise = do
            byte <- readExactly stream 1
            go (skipped + 1) (BS.tail window <> byte)
  go 0 first

readEncrypted :: (PeerStream a) => a -> RC4.State -> Int -> IO (RC4.State, ByteString)
readEncrypted stream state count = RC4.combine state <$> readExactly stream count

readPadding :: (PeerStream a) => a -> RC4.State -> ByteString -> IO RC4.State
readPadding stream state encodedLength = do
  let size = fromIntegral (bytesToInteger encodedLength)
  check (size <= maxPadding) "padding exceeds 512 bytes"
  fst <$> readEncrypted stream state size

newMSE :: a -> Maybe RC4.State -> Maybe RC4.State -> ByteString -> IO (MSE a)
newMSE stream incoming outgoing initial =
  MSE stream <$> newMVar (incoming, initial) <*> newMVar outgoing

validateInfoHash :: ByteString -> IO ()
validateInfoHash skey = check (BS.length skey == 20) "info hash must be 20 bytes"

sendPublicKey :: (PeerStream a) => a -> Integer -> IO ()
sendPublicKey stream private = do
  public <- fromEither (generatePublicKey dhPrime private)
  padding <- randomPadding
  writeBytes stream (public <> padding)

initiate :: (PeerStream a) => ByteString -> a -> IO (MSE a)
initiate skey stream = do
  validateInfoHash skey
  private <- generatePrivateExponent
  sendPublicKey stream private
  remote <- readExactly stream dhKeySize
  secret <- fromEither (deriveSharedSecret dhPrime private remote)
  let outgoing = cipher "keyA" secret skey
      incoming = cipher "keyB" secret skey
      request =
        SHA1.hash ("req1" <> secret)
          <> xorBytes (SHA1.hash ("req2" <> skey)) (SHA1.hash ("req3" <> secret))
  padding <- zeroPadding
  let (sent, encrypted) =
        RC4.combine
          outgoing
          (vc <> method 2 <> word16 (BS.length padding) <> padding <> word16 0)
  writeBytes stream (request <> encrypted)
  let (afterVC, encryptedVC) = RC4.combine incoming vc
  synchronize stream encryptedVC
  (headerState, header) <- readEncrypted stream afterVC 6
  check (bytesToInteger (BS.take 4 header) .&. 3 == 2) "unoffered crypto selection"
  received <- readPadding stream headerState (BS.drop 4 header)
  newMSE stream (Just received) (Just sent) BS.empty

respond :: (PeerStream a) => ByteString -> a -> IO (Either ByteString (MSE a))
respond skey stream = do
  validateInfoHash skey
  (protocol, prefix) <- detectProtocol stream
  case protocol of
    IncomingPlaintext -> pure (Left prefix)
    IncomingMSE -> do
      rest <- readExactly stream (dhKeySize - BS.length prefix)
      private <- generatePrivateExponent
      secret <- fromEither (deriveSharedSecret dhPrime private (prefix <> rest))
      sendPublicKey stream private
      synchronize stream (SHA1.hash ("req1" <> secret))
      identifier <- readExactly stream 20
      check
        (identifier == xorBytes (SHA1.hash ("req2" <> skey)) (SHA1.hash ("req3" <> secret)))
        "unknown torrent info hash"
      let incoming = cipher "keyA" secret skey
          outgoing = cipher "keyB" secret skey
      (headerState, header) <- readEncrypted stream incoming 14
      check (BS.take 8 header == vc) "invalid verification constant"
      let offered = bytesToInteger (BS.take 4 (BS.drop 8 header))
          selected = if offered .&. 2 /= 0 then 2 else 1
      check (offered .&. 3 /= 0) "no supported crypto method"
      afterPadding <- readPadding stream headerState (BS.drop 12 header)
      (lengthState, initialLength) <- readEncrypted stream afterPadding 2
      (received, initial) <- readEncrypted stream lengthState (fromIntegral (bytesToInteger initialLength))
      padding <- zeroPadding
      let (sent, encrypted) =
            RC4.combine
              outgoing
              (vc <> method selected <> word16 (BS.length padding) <> padding)
      writeBytes stream encrypted
      let payloadCipher state = if selected == 2 then Just state else Nothing
      Right <$> newMSE stream (payloadCipher received) (payloadCipher sent) initial

instance (PeerStream a) => PeerStream (MSE a) where
  readBytes (MSE stream incoming _) count = do
    check (count >= 0) "negative read length"
    if count == 0
      then pure BS.empty
      else modifyMVar incoming $ \(state, buffered) -> do
        let (prefix, remaining) = BS.splitAt count buffered
        wire <- readExactly stream (count - BS.length prefix)
        let (next, plaintext) = case state of
              Nothing -> (Nothing, wire)
              Just current -> let (updated, bytes) = RC4.combine current wire in (Just updated, bytes)
        pure ((next, remaining), prefix <> plaintext)
  writeBytes (MSE stream _ outgoing) bytes =
    when (not (BS.null bytes)) $
      modifyMVar outgoing $ \state -> do
        let (next, wire) = case state of
              Nothing -> (Nothing, bytes)
              Just current -> let (updated, encrypted) = RC4.combine current bytes in (Just updated, encrypted)
        writeBytes stream wire
        pure (next, ())
