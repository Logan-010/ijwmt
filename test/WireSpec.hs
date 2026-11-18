{-# LANGUAGE OverloadedStrings #-}

module WireSpec (tests) where

import Data.BitTorrent.Wire
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString, word32BE)
import qualified Data.ByteString.Lazy as BL
import TestSupport

tests :: [Test]
tests =
  [ ("handshake: decode a protocol fixture", assertEqual (Right (reserved, infoHash, peerId)) (decodeHandshake handshake)),
    ("handshake: encode a protocol fixture", assertEqual handshake (encodeHandshake reserved infoHash peerId)),
    ("handshake: round trip binary fields", assertEqual (Right (reserved, infoHash, peerId)) (decodeHandshake (encodeHandshake reserved infoHash peerId))),
    ("reserved: no flags", assertEqual (BS.replicate 8 0) (generateReserved [])),
    ("reserved: all flags", assertEqual reserved (generateReserved [minBound .. maxBound])),
    ("reserved: duplicate flags are idempotent", assertEqual (generateReserved [DHT]) (generateReserved [DHT, DHT])),
    ("peer ID: exactly 20 bytes", generatePeerId >>= assertEqual 20 . BS.length)
  ]
    ++ [ ("reserved: " ++ show flag, assertEqual expected (generateReserved [flag]))
       | (flag, expected) <-
           [ (DHT, BS.pack [0, 0, 0, 0, 0, 0, 0, 1]),
             (FastExtension, BS.pack [0, 0, 0, 0, 0, 0, 0, 4]),
             (ExtensionProtocol, BS.pack [0, 0, 0, 0, 0, 16, 0, 0]),
             (BitTorrentV2, BS.pack [0, 0, 0, 0, 0, 0, 0, 16])
           ]
       ]
    ++ [ ("handshake: reject " ++ label, assertLeft (decodeHandshake raw))
       | (label, raw) <-
           [("truncation at " ++ show n, BS.take n handshake) | n <- [0 .. 67]]
             ++ [ ("trailing bytes", handshake <> "x"),
                  ("wrong protocol length", BS.cons 18 (BS.tail handshake)),
                  ("wrong protocol name", BS.take 1 handshake <> "NotTorrent protocol" <> BS.drop 20 handshake),
                  ("four-byte protocol length", word 19 <> BS.drop 1 handshake)
                ]
       ]
    ++ [ ("handshake: reject invalid encoder field " ++ show i, assertThrows (encodeHandshake r h p))
       | (i, (r, h, p)) <- zip [1 :: Int ..] [("", infoHash, peerId), (reserved, "", peerId), (reserved, infoHash, "")]
       ]
    ++ concat
      [ [ ("wire: encode " ++ show msg, assertEqual raw (encodeMessage msg)),
          ("wire: decode " ++ show msg, assertEqual (Right msg) (decodeMessage raw)),
          ("wire: round trip " ++ show msg, assertEqual (Right msg) (decodeMessage (encodeMessage msg)))
        ]
      | (msg, raw) <- messages
      ]
    ++ [ ("wire: reject truncation " ++ show (msg, n), assertLeft (decodeMessage (BS.take n raw)))
       | (msg, raw) <- messages,
         n <- [0 .. BS.length raw - 1]
       ]
    ++ [ ("wire: reject trailing data " ++ show msg, assertLeft (decodeMessage (raw <> "x")))
       | (msg, raw) <- messages
       ]
    ++ [ ("wire: reject malformed payload " ++ show payload, assertLeft (decodeMessage (frame payload)))
       | payload <-
           [BS.pack [code, 0] | code <- [0 .. 3]]
             ++ [ BS.singleton 4,
                  BS.cons 4 (BS.replicate 5 0),
                  BS.cons 6 (BS.replicate 11 0),
                  BS.cons 6 (BS.replicate 13 0),
                  BS.cons 7 (BS.replicate 7 0),
                  BS.cons 8 (BS.replicate 11 0),
                  BS.cons 8 (BS.replicate 13 0),
                  BS.singleton 255
                ]
             ++ [BS.cons code (BS.replicate n 0) | code <- [21, 23], n <- [47, 49]]
             ++ [BS.cons 22 (BS.replicate n 0) | n <- [47, 49, 79, 81]]
       ]
    ++ [ ("wire: reject invalid hash encoder " ++ show msg, assertThrows (encodeMessage msg))
       | msg <- [HashRequest ("", 0, 0, 0, 0), HashReject ("", 0, 0, 0, 0), Hashes ("", 0, 0, 0, 0, []), Hashes (root, 0, 0, 0, 0, ["short"])]
       ]
  where
    reserved = BS.pack [0, 0, 0, 0, 0, 16, 0, 21]
    infoHash = BS.pack [0 .. 19]
    peerId = BS.pack [236 .. 255]
    handshake = BS.singleton 19 <> "BitTorrent protocol" <> reserved <> infoHash <> peerId
    root = BS.replicate 32 0xaa
    hash1 = BS.replicate 32 0xbb
    hash2 = BS.replicate 32 0xcc
    hashPayload = root <> word 1 <> word 2 <> word 3 <> word 4
    messages =
      [ (KeepAlive, word 0),
        (Choke, frame (BS.singleton 0)),
        (Unchoke, frame (BS.singleton 1)),
        (Interested, frame (BS.singleton 2)),
        (NotInterested, frame (BS.singleton 3)),
        (Have 0x01020304, BS.pack [0, 0, 0, 5, 4, 1, 2, 3, 4]),
        (Have 0, frame (BS.singleton 4 <> word 0)),
        (BitField "", frame (BS.singleton 5)),
        (BitField (BS.pack [0, 128, 255]), frame (BS.pack [5, 0, 128, 255])),
        (Request (1, 16384, 32768), frame (BS.singleton 6 <> word 1 <> word 16384 <> word 32768)),
        (Piece (2, 16384, "data"), frame (BS.singleton 7 <> word 2 <> word 16384 <> "data")),
        (Piece (0, 0, ""), frame (BS.singleton 7 <> word 0 <> word 0)),
        (Cancel (1, 16384, 32768), frame (BS.singleton 8 <> word 1 <> word 16384 <> word 32768)),
        (HashRequest (root, 1, 2, 3, 4), frame (BS.singleton 21 <> hashPayload)),
        (Hashes (root, 1, 2, 3, 4, []), frame (BS.singleton 22 <> hashPayload)),
        (Hashes (root, 1, 2, 3, 4, [hash1, hash2]), frame (BS.singleton 22 <> hashPayload <> hash1 <> hash2)),
        (HashReject (root, 1, 2, 3, 4), frame (BS.singleton 23 <> hashPayload))
      ]

word :: Int -> ByteString
word = BL.toStrict . toLazyByteString . word32BE . fromIntegral

frame :: ByteString -> ByteString
frame payload = word (BS.length payload) <> payload
