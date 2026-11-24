{-# LANGUAGE OverloadedStrings #-}

module ExtensionSpec (tests) where

import Data.BitTorrent.Extension
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import TestSupport

tests :: [Test]
tests =
  [ ("extension handshake: advertise ut_metadata", assertEqual enabled (encodeHandshake [Metadata])),
    ("extension handshake: disable ut_metadata", assertEqual disabled (encodeHandshake [])),
    ("extension handshake: duplicate capabilities", assertEqual enabled (encodeHandshake [Metadata, Metadata])),
    ("extension handshake: decode enabled", assertEqual (Just [(Metadata, 1)]) (decodeHandshake enabled)),
    ("extension handshake: decode disabled", assertEqual (Just [(Metadata, 0)]) (decodeHandshake disabled)),
    ("extension handshake: remote extension ID", assertEqual (Just [(Metadata, 7)]) (decodeHandshake "d1:md11:ut_metadatai7eee")),
    ("extension handshake: ignore unknown capabilities", assertEqual (Just []) (decodeHandshake "d1:md7:unknowni2eee")),
    ("extension handshake: empty capabilities", assertEqual (Just []) (decodeHandshake "d1:mdee")),
    ("ut_metadata: ignore unknown header fields", assertEqual (Just (UtMetadata (Request 2))) (decode "d5:extrai9e8:msg_typei0e5:piecei2ee"))
  ]
    ++ [ ("extension handshake: reject " ++ show raw, assertEqual Nothing (decodeHandshake raw))
       | raw <- ["", "d", "le", "de", "d1:mi1ee", enabled <> "x"]
       ]
    ++ concat
      [ [ ("ut_metadata: encode " ++ show msg, assertEqual (1, raw) (encode msg)),
          ("ut_metadata: decode " ++ show msg, assertEqual (Just msg) (decode raw)),
          ("ut_metadata: round trip " ++ show msg, assertEqual (Just msg) (decode (snd (encode msg))))
        ]
      | (msg, raw) <- messages
      ]
    ++ [ ("ut_metadata: reject truncation " ++ show (msg, n), assertEqual Nothing (decode (BS.take n raw)))
       | (msg, raw) <- messages,
         n <- [0 .. BS.length raw - 1]
       ]
    ++ [ ("ut_metadata: reject malformed message " ++ show raw, assertEqual Nothing (decode raw))
       | raw <-
           [ "x",
             "i1e",
             "le",
             "de",
             "d5:piecei0ee",
             "d8:msg_typei0ee",
             "d8:msg_type1:x5:piecei0ee",
             "d8:msg_typei0e5:piece1:xe",
             "d8:msg_typei3e5:piecei0ee",
             "d8:msg_typei-1e5:piecei0ee",
             "d8:msg_typei1e5:piecei0ee",
             "d8:msg_typei1e5:piecei0e10:total_size1:xe",
             "d8:msg_typei1e5:piecei0e10:total_sizei-1ee",
             "d8:msg_typei1e5:piecei0e10:total_sizei3eeab",
             "d8:msg_typei1e5:piecei0e10:total_sizei3eeabcd",
             "d8:msg_typei1e5:piecei0e10:total_sizei0eex"
           ]
       ]
  where
    enabled = "d1:md11:ut_metadatai1eee"
    disabled = "d1:md11:ut_metadatai0eee"

messages :: [(Message, ByteString)]
messages =
  [ (UtMetadata (Request 0), "d8:msg_typei0e5:piecei0ee"),
    (UtMetadata (Request 42), "d8:msg_typei0e5:piecei42ee"),
    (UtMetadata (Reject 0), "d8:msg_typei2e5:piecei0ee"),
    (UtMetadata (Reject 42), "d8:msg_typei2e5:piecei42ee"),
    (UtMetadata (Data 0 ""), "d8:msg_typei1e5:piecei0e10:total_sizei0ee"),
    (UtMetadata (Data 2 "spam"), "d8:msg_typei1e5:piecei2e10:total_sizei4eespam"),
    (UtMetadata (Data 1 binary), "d8:msg_typei1e5:piecei1e10:total_sizei6ee" <> binary),
    (UtMetadata (Data 0 block), "d8:msg_typei1e5:piecei0e10:total_sizei16384ee" <> block)
  ]
  where
    binary = BS.pack [0, 128, 255, 100, 101, 105]
    block = BS.replicate 16384 0xff
