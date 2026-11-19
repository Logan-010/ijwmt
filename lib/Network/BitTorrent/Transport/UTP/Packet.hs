module Network.BitTorrent.Transport.UTP.Packet where

import Control.Monad (when)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, toLazyByteString, word8, word16BE, word32BE)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word16, Word32)

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
