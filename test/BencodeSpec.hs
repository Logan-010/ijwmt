{-# LANGUAGE OverloadedStrings #-}

module BencodeSpec (tests) where

import Data.BitTorrent.Bencode
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import TestSupport

tests :: [Test]
tests =
  [ ("bencode: decode " ++ show raw, assertEqual (Right value) (decode raw))
  | (raw, value) <- examples
  ]
    ++ [ ("bencode: encode " ++ show value, assertEqual raw (encode value))
       | (raw, value) <- examples
       ]
    ++ [ ("bencode: round trip " ++ show value, assertEqual (Right value) (decode (encode value)))
       | value <- map snd examples ++ [BInt (10 ^ (100 :: Int)), BInt (negate (10 ^ (100 :: Int)))]
       ]
    ++ [ ("bencode: reject " ++ show raw, assertLeft (decode raw))
       | raw <-
           [ "",
             "x",
             "e",
             "i",
             "ie",
             "i1",
             "i-0e",
             "i01e",
             "i-01e",
             "i+1e",
             "i1.0e",
             "i 1e",
             "3foo",
             "3:ab",
             "-1:a",
             "999999999999999999999999999999:a",
             "l",
             "li1e",
             "d",
             "d1:a",
             "di1ei2ee",
             "i1ejunk",
             "0:0:",
             "lee"
           ]
       ]
  where
    examples =
      [ ("0:", BString ""),
        ("4:spam", BString "spam"),
        (BS.pack [51, 58, 0, 128, 255], BString (BS.pack [0, 128, 255])),
        ("i0e", BInt 0),
        ("i42e", BInt 42),
        ("i-42e", BInt (-42)),
        ("le", BList []),
        ("l4:spami42elee", BList [BString "spam", BInt 42, BList []]),
        ("de", BDict Map.empty),
        ("d1:ai1e1:b4:spame", BDict (Map.fromList [("b", BString "spam"), ("a", BInt 1)])),
        ("d1:ald1:bi2eeee", BDict (Map.singleton "a" (BList [BDict (Map.singleton "b" (BInt 2))])))
      ]
