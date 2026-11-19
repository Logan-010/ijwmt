{-# LANGUAGE OverloadedStrings #-}

module TrackerHTTPSpec (tests) where

import Control.Exception (IOException, try)
import qualified Data.BitTorrent.Bencode as BE
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Network.BitTorrent.Tracker
import qualified Network.BitTorrent.Tracker.HTTP as HTTP
import Network.Socket (SockAddr (..), tupleToHostAddress, tupleToHostAddress6)
import System.IO.Error (ioeGetErrorString)
import TestSupport

tests :: [Test]
tests =
  [ ("tracker HTTP: compact IPv4 and IPv6", check (TrackerResponse 1800 (Just 7) (Just 11) [peer4, peer6])
        [("complete", BE.BInt 7), ("incomplete", BE.BInt 11), ("peers", BE.BString compact4), ("peers6", BE.BString compact6)]),
    ("tracker HTTP: non-compact IPv4 and IPv6 with optional IDs", check (response [peer4 {peerId = Just "-IJ0001-123456789012"}, peer6])
        [("peers", BE.BList [entry "192.0.2.1" 6881 [("peer id", BE.BString "-IJ0001-123456789012")], entry "2001:db8::1" 51413 []])]),
    ("tracker HTTP: combine non-compact and compact IPv6", check (response [peer4, peer6])
        [("peers", BE.BList [entry "192.0.2.1" 6881 []]), ("peers6", BE.BString compact6)]),
    ("tracker HTTP: multiple compact peers and network-order ports", check (response [peer4, Peer Nothing (SockAddrInet 65535 (tupleToHostAddress (203, 0, 113, 254)))])
        [("peers", BE.BString (compact4 <> BS.pack [203, 0, 113, 254, 255, 255]))]),
    ("tracker HTTP: IPv6 only", check (response [peer6]) [("peers6", BE.BString compact6)]),
    ("tracker HTTP: empty peer lists", check (response []) [("peers", BE.BString ""), ("peers6", BE.BString "")]),
    ("tracker HTTP: empty non-compact list", check (response []) [("peers", BE.BList [])]),
    ("tracker HTTP: missing peer fields", check (response []) []),
    ("tracker HTTP: ignore extension fields", check (response []) [("warning message", BE.BString "warning"), ("min interval", BE.BInt 60)]),
    ("tracker HTTP: failure reason preserved", do
        result <- try (HTTP.decode (BE.encode (dict [("failure reason", BE.BString "not registered")]))) :: IO (Either IOException TrackerResponse)
        case result of
          Left err -> assertEqual "Invalid tracker response: not registered" (ioeGetErrorString err)
          Right value -> fail ("Expected tracker failure, got " ++ show value))
  ]
    ++ [("tracker HTTP: reject " ++ label, rejected (BE.encode value)) | (label, value) <- invalid]
    ++ [("tracker HTTP: reject truncated compact IPv4 " ++ show n, rejected (payload [("peers", BE.BString (BS.take n compact4))])) | n <- [1 .. 5]]
    ++ [("tracker HTTP: reject truncated compact IPv6 " ++ show n, rejected (payload [("peers6", BE.BString (BS.take n compact6))])) | n <- [1 .. 17]]
    ++ [("tracker HTTP: reject malformed bencode", rejected "d8:intervali1e")]
  where
    check expected fields = HTTP.decode (payload fields) >>= assertEqual expected
    invalid =
      [ ("non-dictionary response", BE.BList []),
        ("missing interval", dict []),
        ("string interval", dict [("interval", BE.BString "1800")]),
        ("negative interval", dict [("interval", BE.BInt (-1))]),
        ("overflow interval", dict [("interval", BE.BInt (toInteger (maxBound :: Int) + 1))]),
        ("invalid complete", withInterval [("complete", BE.BInt (-1))]),
        ("invalid incomplete", withInterval [("incomplete", BE.BString "1")]),
        ("invalid peers", withInterval [("peers", BE.BInt 0)]),
        ("non-string peers6", withInterval [("peers6", BE.BList [])]),
        ("invalid failure reason", dict [("failure reason", BE.BInt 1)]),
        ("non-dictionary peer", withInterval [("peers", BE.BList [BE.BString "peer"])]),
        ("missing peer IP", withInterval [("peers", BE.BList [dict [("port", BE.BInt 6881)]])]),
        ("missing peer port", withInterval [("peers", BE.BList [dict [("ip", BE.BString "127.0.0.1")]])])
      ]
        ++ [("peer " ++ label, withInterval [("peers", BE.BList [entry address port extras])])
           | (label, address, port, extras) <-
               [("invalid IP", "invalid", 6881, []), ("hostname", "localhost", 6881, []), ("empty IP", "", 6881, []),
                ("embedded NUL", "127.0.0.1\0junk", 6881, []), ("negative port", "127.0.0.1", -1, []),
                ("overflow port", "::1", 65536, []), ("invalid ID", "::1", 6881, [("peer id", BE.BInt 1)])]
           ]

response :: [Peer] -> TrackerResponse
response = TrackerResponse 1800 Nothing Nothing

peer4, peer6 :: Peer
peer4 = Peer Nothing (SockAddrInet 6881 (tupleToHostAddress (192, 0, 2, 1)))
peer6 = Peer Nothing (SockAddrInet6 51413 0 (tupleToHostAddress6 (0x2001, 0xdb8, 0, 0, 0, 0, 0, 1)) 0)

compact4, compact6 :: BS.ByteString
compact4 = BS.pack [192, 0, 2, 1, 0x1a, 0xe1]
compact6 = BS.pack [0x20, 1, 0xd, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0xc8, 0xd5]

dict :: [(BS.ByteString, BE.BencodeValue)] -> BE.BencodeValue
dict = BE.BDict . Map.fromList

withInterval :: [(BS.ByteString, BE.BencodeValue)] -> BE.BencodeValue
withInterval = dict . (("interval", BE.BInt 1800) :)

payload :: [(BS.ByteString, BE.BencodeValue)] -> BS.ByteString
payload = BE.encode . withInterval

entry :: BS.ByteString -> Integer -> [(BS.ByteString, BE.BencodeValue)] -> BE.BencodeValue
entry address port extras = dict ([("ip", BE.BString address), ("port", BE.BInt port)] ++ extras)

rejected :: BS.ByteString -> IO ()
rejected bytes = do
  result <- try (HTTP.decode bytes) :: IO (Either IOException TrackerResponse)
  case result of
    Left _ -> pure ()
    Right value -> fail ("Expected rejection, got " ++ show value)
