{-# LANGUAGE OverloadedStrings #-}

module TorrentSpec (tests) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import Data.BitTorrent
import Data.BitTorrent.Bencode (BencodeValue (..))
import qualified Data.BitTorrent.Bencode as Bencode
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import TestSupport

tests :: [Test]
tests =
  [ ("torrent: v1 single file and defaults", assertEqual (Right (expected (V1 (v1 singleInfo [file] True)))) (parse singleInfo [])),
    ("torrent: v1 multiple files", assertEqual (Right (expected (V1 (v1 multiInfo multiFiles False)))) (parse multiInfo [])),
    ("torrent: v1 empty file", assertEqual (Right (expected (V1 (v1 emptyInfo [TorrentFile ["sample"] 0 noAttributes Nothing Nothing Nothing] True)))) (parse emptyInfo [])),
    ("torrent: v2 file tree", assertEqual (Right (expected (V2 (v2 v2Info [v2File] Map.empty)))) (parse v2Info [])),
    ("torrent: v2 nested tree and empty file", assertEqual (Right (expected (V2 (v2 nestedInfo [nestedFile] Map.empty)))) (parse nestedInfo [])),
    ("torrent: v2 piece layers", assertEqual (Right (expected (V2 (v2 layeredInfo [largeFile] layers)))) (parse layeredInfo layerFields)),
    ("torrent: hybrid", assertEqual (Right (expected (Hybrid (v1 hybridInfo [file] True) (v2 hybridInfo [v2File] Map.empty)))) (parse hybridInfo [])),
    ("torrent: optional metadata", metadataTest),
    ( "torrent: single web seed",
      do
        torrent <- either fail pure (parse singleInfo [("url-list", BString "https://seed/file")])
        assertEqual ["https://seed/file"] (webSeeds torrent)
    ),
    ("torrent: file attributes, symlink and checksums", attributesTest),
    ( "torrent: binary sha1",
      do
        torrent <- either fail pure (parse (set "sha1" (BString pieceHash) singleInfo) [])
        case version torrent of
          V1 (TorrentV1 _ _ [TorrentFile _ _ _ _ checksum _] _) -> assertEqual (Just pieceHash) checksum
          other -> fail ("Unexpected version: " ++ show other)
    ),
    ( "torrent: top-level metadata does not change info hash",
      do
        first <- either fail pure (parse singleInfo [])
        second <- either fail pure (parse singleInfo [("comment", BString "different")])
        assertEqual (version first) (version second)
    )
  ]
    ++ [ ("torrent: reject " ++ label, assertLeft (parse info top))
       | (label, info, top) <- invalid
       ]
    ++ [ ("torrent: reject top level " ++ show value, assertLeft (decode value))
       | value <- [BInt 1, BList [], dict [], dict [("info", BString "bad")]]
       ]
  where
    file = TorrentFile ["sample"] 3 noAttributes Nothing Nothing Nothing
    v2File = TorrentFile ["sample"] 3 noAttributes Nothing Nothing (Just root)
    multiFiles =
      [ TorrentFile ["dir", "a"] 1 noAttributes Nothing Nothing Nothing,
        TorrentFile ["b"] 2 noAttributes Nothing Nothing Nothing
      ]
    multiInfo = set "files" (BList [dict [("path", BList [BString "dir", BString "a"]), ("length", BInt 1)], dict [("path", BList [BString "b"]), ("length", BInt 2)]]) (remove "length" singleInfo)
    emptyInfo = set "pieces" (BString "") (set "length" (BInt 0) singleInfo)
    nestedFile = TorrentFile ["dir", "empty"] 0 noAttributes Nothing Nothing Nothing
    nestedInfo = set "file tree" (dict [("dir", dict [("empty", dict [("", dict [("length", BInt 0)])])])]) v2Info
    largeFile = TorrentFile ["sample"] 16385 noAttributes Nothing Nothing (Just root)
    layeredInfo = set "file tree" (tree (dict [("length", BInt 16385), ("pieces root", BString root)])) v2Info
    layerHashes = [BS.replicate 32 0x11, BS.replicate 32 0x22]
    layers = Map.singleton root layerHashes
    layerFields = [("piece layers", dict [(root, BString (BS.concat layerHashes))])]
    hybridInfo = set "pieces" (BString pieceHash) (set "length" (BInt 3) v2Info)

    metadataTest = do
      let info = set "source" (BString "source") (set "private" (BInt 1) singleInfo)
          top =
            [ ("announce", BString "https://tracker/announce"),
              ("announce-list", BList [BList [BString "https://tracker/announce"], BList [BString "udp://backup"]]),
              ("url-list", BList [BString "https://seed/one", BString "https://seed/two"]),
              ("comment", BString "hello"),
              ("created by", BString "ijwmt"),
              ("creation date", BInt 123456789)
            ]
          wanted =
            (expected (V1 (v1 info [file] True)))
              { announce = Just "https://tracker/announce",
                announceList = [["https://tracker/announce"], ["udp://backup"]],
                webSeeds = ["https://seed/one", "https://seed/two"],
                comment = Just "hello",
                createdBy = Just "ijwmt",
                creationDate = Just 123456789,
                private = True,
                source = Just "source"
              }
      assertEqual (Right wanted) (parse info top)

    attributesTest = do
      let entries =
            [ dict [("path", BList [BString "data"]), ("length", BInt 3), ("attr", BString "pxh"), ("sha1", BString "00112233445566778899aAbBcCdDeEfF00112233")],
              dict [("path", BList [BString "link"]), ("attr", BString "l"), ("symlink path", BList [BString "data"])]
            ]
          info = set "files" (BList entries) (remove "length" singleInfo)
          checksum = BS.pack [0, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0, 0x11, 0x22, 0x33]
          wantedFiles =
            [ TorrentFile ["data"] 3 (FileAttributes True True True False) Nothing (Just checksum) Nothing,
              TorrentFile ["link"] 0 (FileAttributes False False False True) (Just ["data"]) Nothing Nothing
            ]
      assertEqual (Right (expected (V1 (v1 info wantedFiles False)))) (parse info [])

    invalid =
      [("missing " ++ show key, remove key singleInfo, []) | key <- ["name", "piece length", "pieces", "length"]]
        ++ [("unsafe name " ++ show nameValue, set "name" (BString nameValue) singleInfo, []) | nameValue <- ["", ".", "..", "a/b", "a\\b", BS.pack [0], BS.pack [255]]]
        ++ [("non-positive piece length " ++ show n, set "piece length" (BInt n) singleInfo, []) | n <- [0, -1]]
        ++ [ ("non-dictionary info", BList [], []),
             ("wrong name type", set "name" (BInt 1) singleInfo, []),
             ("wrong pieces type", set "pieces" (BList []) singleInfo, []),
             ("invalid piece hash width", set "pieces" (BString "short") singleInfo, []),
             ("wrong piece count", set "pieces" (BString "") singleInfo, []),
             ("negative file length", set "length" (BInt (-1)) singleInfo, []),
             ("both files and length", set "files" (BList []) singleInfo, []),
             ("empty files", set "files" (BList []) (remove "length" singleInfo), []),
             ("invalid private flag", set "private" (BInt 2) singleInfo, []),
             ("unsupported meta version", set "meta version" (BInt 3) singleInfo, []),
             ("invalid sha1 width", set "sha1" (BString "short") singleInfo, []),
             ("invalid sha1 hex", set "sha1" (BString (BS.replicate 40 0x67)) singleInfo, []),
             ("missing symlink path", set "attr" (BString "l") singleInfo, []),
             ("invalid tracker type", singleInfo, [("announce", BInt 1)]),
             ("invalid tracker tiers", singleInfo, [("announce-list", BList [BString "tracker"])]),
             ("invalid web seed type", singleInfo, [("url-list", BInt 1)]),
             ("invalid creation date", singleInfo, [("creation date", BString "today")]),
             ("small v2 piece length", set "piece length" (BInt 8192) v2Info, []),
             ("non-power-of-two v2 piece length", set "piece length" (BInt 20000) v2Info, []),
             ("missing file tree", remove "file tree" v2Info, []),
             ("empty file tree", set "file tree" (dict []) v2Info, []),
             ("empty tree directory", set "file tree" (dict [("dir", dict [])]) v2Info, []),
             ("root tree leaf", set "file tree" (dict [("", dict [("length", BInt 0)])]) v2Info, []),
             ("mixed leaf and directory", set "file tree" (dict [("sample", dict [("", dict [("length", BInt 0)]), ("child", dict [])])]) v2Info, []),
             ("missing pieces root", set "file tree" (tree (dict [("length", BInt 3)])) v2Info, []),
             ("invalid pieces root", set "file tree" (tree (dict [("length", BInt 3), ("pieces root", BString "short")])) v2Info, []),
             ("missing piece layer", layeredInfo, []),
             ("wrong layer count", layeredInfo, [("piece layers", dict [(root, BString root)])]),
             ("invalid layer hash width", layeredInfo, [("piece layers", dict [(root, BString "short")])]),
             ("invalid layer root width", v2Info, [("piece layers", dict [("short", BString root)])]),
             ("invalid hybrid v1 data", set "pieces" (BString "short") hybridInfo, [])
           ]
        ++ [ ("invalid file path " ++ show components, set "files" (BList [dict [("path", BList (map BString components)), ("length", BInt 3)]]) (remove "length" singleInfo), [])
           | components <- [[], [".."], [""], ["a/b"], ["a\\b"], [BS.pack [0]], [BS.pack [255]]]
           ]

noAttributes :: FileAttributes
noAttributes = FileAttributes False False False False

dict :: [(ByteString, BencodeValue)] -> BencodeValue
dict = BDict . Map.fromList

set :: ByteString -> BencodeValue -> BencodeValue -> BencodeValue
set key value (BDict values) = BDict (Map.insert key value values)
set _ _ value = error ("Expected dictionary fixture: " ++ show value)

remove :: ByteString -> BencodeValue -> BencodeValue
remove key (BDict values) = BDict (Map.delete key values)
remove _ value = error ("Expected dictionary fixture: " ++ show value)

pieceHash :: ByteString
pieceHash = BS.pack [0 .. 19]

root :: ByteString
root = BS.pack [0 .. 31]

singleInfo :: BencodeValue
singleInfo = dict [("name", BString "sample"), ("piece length", BInt 16384), ("length", BInt 3), ("pieces", BString pieceHash)]

tree :: BencodeValue -> BencodeValue
tree leaf = dict [("sample", dict [("", leaf)])]

v2Info :: BencodeValue
v2Info = dict [("name", BString "sample"), ("piece length", BInt 16384), ("meta version", BInt 2), ("file tree", tree (dict [("length", BInt 3), ("pieces root", BString root)]))]

-- Exercise the complete .torrent bytes -> bencode -> torrent pipeline.
parse :: BencodeValue -> [(ByteString, BencodeValue)] -> Either String Torrent
parse info top = Bencode.decode (Bencode.encode (dict (("info", info) : top))) >>= decode

expected :: TorrentVersion -> Torrent
expected = Torrent "sample" Nothing [] [] Nothing Nothing Nothing 16384 False Nothing

v1 :: BencodeValue -> [TorrentFile] -> Bool -> TorrentV1
v1 info torrentFiles single = TorrentV1 (SHA1.hash (Bencode.encode info)) hashes torrentFiles single
  where
    hashes = case info of
      BDict values | Map.lookup "pieces" values == Just (BString "") -> []
      _ -> [pieceHash]

v2 :: BencodeValue -> [TorrentFile] -> Map.Map ByteString [ByteString] -> TorrentV2
v2 info = TorrentV2 (SHA256.hash (Bencode.encode info))
