module MagnetSpec (tests) where

import Data.BitTorrent.Magnet
import qualified Data.ByteString as BS
import Data.Char (toLower, toUpper)
import Network.URL (URL, importURL)
import TestSupport

tests :: [Test]
tests =
  [ ("magnet: v1 hexadecimal and defaults", assertEqual (Right v1Magnet) (decode v1Link)),
    ("magnet: uppercase v1 hexadecimal", assertEqual (Right v1Magnet) (decode (link ("urn:btih:" ++ map toUpper v1Hex)))),
    ("magnet: v2 multihash and defaults", assertEqual (Right (expected [v2Topic])) (decode v2Link)),
    ("magnet: uppercase v2 digest", assertEqual (Right (expected [v2Topic])) (decode (link ("urn:btmh:1220" ++ map toUpper v2Hex)))),
    ("magnet: hybrid topics preserve order", assertEqual (Right (expected [v1Topic, v2Topic])) (decode (v1Link ++ "&xt=urn:btmh:1220" ++ v2Hex))),
    ("magnet: reverse hybrid topics preserve order", assertEqual (Right (expected [v2Topic, v1Topic])) (decode (v2Link ++ "&xt=urn:btih:" ++ v1Hex))),
    ("magnet: repeated topics", assertEqual (Right (expected [v1Topic, v1Topic])) (decode (v1Link ++ "&xt=urn:btih:" ++ v1Hex))),
    ("magnet: percent-encoded topic", assertEqual (Right v1Magnet) (decode ("magnet:?xt=urn%3Abtih%3A" ++ v1Hex))),
    ("magnet: optional fields", optionalFieldsTest),
    ("magnet: repeated fields preserve order", repeatedFieldsTest),
    ("magnet: topic need not be first", assertEqual (Right (v1Magnet {displayName = Just "sample"})) (decode ("magnet:?dn=sample&xt=urn:btih:" ++ v1Hex))),
    ("magnet: empty display name", assertEqual (Right (v1Magnet {displayName = Just ""})) (decode (v1Link ++ "&dn=")))
  ]
    ++ [ ("magnet: " ++ label ++ " v1 base32", assertEqual (Right v1Magnet) (decode (link ("urn:btih:" ++ hash))))
       | (label, hash) <- [("uppercase", v1Base32), ("lowercase", map toLower v1Base32), ("mixed-case", "aAAQEAYEAUDAOCAJBIFQYDIOB4IBCEQt")]
       ]
    ++ [ ("magnet: exact size " ++ show size, assertEqual (Right (v1Magnet {exactSize = Just size})) (decode (v1Link ++ "&xl=" ++ show size)))
       | size <- [0, 1, 123456789012345678901234567890]
       ]
    ++ [("magnet: reject " ++ label, assertLeft (decode input)) | (label, input) <- invalid]
  where
    optionalFieldsTest = do
      let wanted =
            v1Magnet
              { displayName = Just "sample file & notes+extras",
                trackerUrl = [url "https://tracker.example/announce?key=one&token=two"],
                exactSize = Just 123,
                webSeed = [url "https://seed.example/file"],
                acceptableSource = [url "https://source.example/file.torrent"],
                exactSource = [url "https://exact.example/file.torrent"],
                keywordTerms = ["sample words"]
              }
      assertEqual (Right wanted) $
        decode $
          v1Link
            ++ "&dn=sample%20file%20%26%20notes%2Bextras"
            ++ "&tr=https%3A%2F%2Ftracker.example%2Fannounce%3Fkey%3Done%26token%3Dtwo"
            ++ "&xl=123&ws=https%3A%2F%2Fseed.example%2Ffile"
            ++ "&as=https%3A%2F%2Fsource.example%2Ffile.torrent"
            ++ "&xs=https%3A%2F%2Fexact.example%2Ffile.torrent&kt=sample%20words"

    repeatedFieldsTest = do
      let first = url "https://first.example/file"
          second = url "https://second.example/file"
          wanted =
            v1Magnet
              { trackerUrl = [first, second],
                webSeed = [first, second],
                acceptableSource = [first, second],
                exactSource = [first, second],
                keywordTerms = ["first", "second", "first"]
              }
          fields = concat ["&" ++ key ++ "=https%3A%2F%2F" ++ host ++ ".example%2Ffile" | host <- ["first", "second"], key <- ["tr", "ws", "as", "xs"]]
      assertEqual (Right wanted) (decode (v1Link ++ fields ++ "&kt=first&kt=second&kt=first"))

    invalid =
      [ ("missing topic", "magnet:?dn=sample"),
        ("empty query", "magnet:?"),
        ("empty topic", link ""),
        ("unsupported topic", link ("urn:sha1:" ++ v1Hex)),
        ("missing topic URN", link v1Hex),
        ("invalid v1 hexadecimal", link ("urn:btih:" ++ replicate 40 'g')),
        ("invalid v1 base32", link ("urn:btih:" ++ replicate 32 '0')),
        ("unsupported v2 multihash", link ("urn:btmh:1120" ++ v2Hex)),
        ("missing v2 multihash prefix", link ("urn:btmh:" ++ v2Hex)),
        ("invalid v2 hexadecimal", link ("urn:btmh:1220" ++ replicate 64 'g')),
        ("invalid topic after valid topic", v1Link ++ "&xt=urn:btih:bad"),
        ("duplicate display name", v1Link ++ "&dn=one&dn=two"),
        ("duplicate empty display name", v1Link ++ "&dn=&dn=two"),
        ("duplicate size", v1Link ++ "&xl=1&xl=2"),
        ("duplicate zero size", v1Link ++ "&xl=0&xl=1"),
        ("empty size", v1Link ++ "&xl="),
        ("non-numeric size", v1Link ++ "&xl=large"),
        ("fractional size", v1Link ++ "&xl=1.5"),
        ("unknown parameter", v1Link ++ "&unknown=value")
      ]
        ++ [("v1 hash length " ++ show n, link ("urn:btih:" ++ replicate n 'a')) | n <- [0, 31, 33, 39, 41]]
        ++ [("v2 digest length " ++ show n, link ("urn:btmh:1220" ++ replicate n 'a')) | n <- [0, 62, 63, 65, 66]]

link :: String -> String
link topicValue = "magnet:?xt=" ++ topicValue

v1Hex, v2Hex, v1Base32, v1Link, v2Link :: String
v1Hex = "000102030405060708090a0b0c0d0e0f10111213"
v2Hex = v1Hex ++ "1415161718191a1b1c1d1e1f"
v1Base32 = "AAAQEAYEAUDAOCAJBIFQYDIOB4IBCEQT"
v1Link = link ("urn:btih:" ++ v1Hex)
v2Link = link ("urn:btmh:1220" ++ v2Hex)

v1Topic, v2Topic :: Topic
v1Topic = V1 (BS.pack [0 .. 19])
v2Topic = V2 (BS.pack [0 .. 31])

expected :: [Topic] -> Magnet
expected topics = Magnet topics Nothing [] Nothing [] [] [] []

v1Magnet :: Magnet
v1Magnet = expected [v1Topic]

url :: String -> URL
url value = case importURL value of
  Just parsed -> parsed
  Nothing -> error ("Invalid URL fixture: " ++ value)
