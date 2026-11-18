{-# LANGUAGE OverloadedStrings #-}

module Data.BitTorrent.Wire (generatePeerId, ReservedFlag (..), generateReserved, encodeHandshake, decodeHandshake, Message (..), encodeMessage, decodeMessage) where

import Data.Bits (Bits ((.|.)), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString, word32BE)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import System.Entropy (getEntropy)

generatePeerId :: IO ByteString
generatePeerId = getEntropy 20

data ReservedFlag
  = DHT
  | FastExtension
  | ExtensionProtocol
  | BitTorrentV2
  deriving (Show, Eq, Enum, Bounded)

generateReserved :: [ReservedFlag] -> ByteString
generateReserved = BS.pack . foldl' setFlag (replicate 8 0)
  where
    setFlag bytes flag =
      let (index, mask) = case flag of
            DHT -> (7, 0x01)
            FastExtension -> (7, 0x04)
            ExtensionProtocol -> (5, 0x10)
            BitTorrentV2 -> (7, 0x10)
       in [ if i == index then b .|. mask else b
          | (i, b) <- zip [0 :: Integer ..] bytes
          ]

encodeHandshake :: ByteString -> ByteString -> ByteString -> ByteString
encodeHandshake reserved infoHash peerId
  | BS.length reserved /= 8 = error "Reserved must be 8 bytes"
  | BS.length infoHash /= 20 = error "Info hash must be 20 bytes"
  | BS.length peerId /= 20 = error "Peer ID must be 20 bytes"
  | otherwise =
      BS.concat
        [ BS.singleton 19,
          BC.pack "BitTorrent protocol",
          reserved,
          infoHash,
          peerId
        ]

decodeHandshake :: ByteString -> Either String (ByteString, ByteString, ByteString)
decodeHandshake h
  | BS.length h /= 68 || BS.head h /= 19 || slice 1 19 h /= "BitTorrent protocol" = Left "Invalid handshake"
  | otherwise =
      Right
        ( slice 20 8 h,
          slice 28 20 h,
          slice 48 20 h
        )

data Message
  = KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have Int
  | BitField ByteString
  | Request (Int, Int, Int)
  | Piece (Int, Int, ByteString)
  | Cancel (Int, Int, Int)
  | Extended (Int, ByteString)
  | SuggestPiece Int
  | HaveAll
  | HaveNone
  | RejectRequest (Int, Int, Int)
  | AllowedFast Int
  | HashRequest (ByteString, Int, Int, Int, Int)
  | Hashes (ByteString, Int, Int, Int, Int, [ByteString])
  | HashReject (ByteString, Int, Int, Int, Int)
  deriving (Show, Eq)

packInt :: Int -> ByteString
packInt n =
  BL.toStrict $
    toLazyByteString (word32BE (fromIntegral n))

packSingleInt :: Int -> ByteString
packSingleInt n = BS.singleton (fromIntegral n)

unpackInt :: BS.ByteString -> Int
unpackInt =
  BS.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0
    . BS.take 4

encodeMessage :: Message -> ByteString
encodeMessage msg = case msg of
  KeepAlive -> BS.replicate 4 0
  Choke -> BS.concat $ packInt 1 : [packSingleInt 0]
  Unchoke -> BS.concat $ packInt 1 : [packSingleInt 1]
  Interested -> BS.concat $ packInt 1 : [packSingleInt 2]
  NotInterested -> BS.concat $ packInt 1 : [packSingleInt 3]
  Have i -> BS.concat $ packInt 5 : packSingleInt 4 : [packInt i]
  BitField b -> let l = BS.length b in BS.concat $ packInt (l + 1) : packSingleInt 5 : [b]
  Request (i, b, l) -> BS.concat $ packInt 13 : packSingleInt 6 : packInt i : packInt b : [packInt l]
  Piece (i, b, c) -> let l = BS.length c in BS.concat $ packInt (9 + l) : packSingleInt 7 : packInt i : packInt b : [c]
  Cancel (i, b, l) -> BS.concat $ packInt 13 : packSingleInt 8 : packInt i : packInt b : [packInt l]
  SuggestPiece i -> BS.concat $ packInt 5 : packSingleInt 13 : [packInt i]
  HaveAll -> BS.concat $ packInt 1 : [packSingleInt 14]
  HaveNone -> BS.concat $ packInt 1 : [packSingleInt 15]
  RejectRequest (i, b, l) -> BS.concat $ packInt 13 : packSingleInt 16 : packInt i : packInt b : [packInt l]
  AllowedFast i -> BS.concat $ packInt 5 : packSingleInt 17 : [packInt i]
  Extended (i, d) -> BS.concat $ packInt (2 + BS.length d) : packSingleInt 20 : packSingleInt i : [d]
  HashRequest (p, b, i, l, pr)
    | BS.length p /= 32 ->
        error "Pieces root must be 32 bytes"
    | otherwise -> BS.concat $ packInt 49 : packSingleInt 21 : p : packInt b : packInt i : packInt l : [packInt pr]
  Hashes (p, b, i, l, pr, hs)
    | BS.length p /= 32 ->
        error "Pieces root must be 32 bytes"
    | any ((/= 32) . BS.length) hs ->
        error "Each hash must be 32 bytes"
    | otherwise ->
        let payload =
              BS.concat $
                p
                  : packInt b
                  : packInt i
                  : packInt l
                  : packInt pr
                  : hs
         in BS.concat
              [ packInt (1 + BS.length payload),
                packSingleInt 22,
                payload
              ]
  HashReject (p, b, i, l, pr)
    | BS.length p /= 32 ->
        error "Pieces root must be 32 bytes"
    | otherwise ->
        BS.concat $ packInt 49 : packSingleInt 23 : p : packInt b : packInt i : packInt l : [packInt pr]

slice :: Int -> Int -> ByteString -> ByteString
slice start len =
  BS.take len . BS.drop start

decodeMessage :: ByteString -> Either String Message
decodeMessage d
  | BS.length d < 4 =
      Left "Missing length prefix"
  | n == 0 =
      if BS.length d == 4
        then Right KeepAlive
        else Left "Invalid keep-alive length"
  | n > BS.length d - 4 =
      Left "Truncated message"
  | n < BS.length d - 4 =
      Left "Extra bytes after message"
  | otherwise =
      let code = BS.index d 4
          msg = BS.drop 5 d
       in decodePayload code msg
  where
    n = unpackInt (BS.take 4 d)

decodePayload :: Word8 -> ByteString -> Either String Message
decodePayload code msg =
  case code of
    0
      | BS.length msg /= 0 -> Left "Invalid message"
      | otherwise -> Right Choke
    1
      | BS.length msg /= 0 -> Left "Invalid message"
      | otherwise -> Right Unchoke
    2
      | BS.length msg /= 0 -> Left "Invalid message"
      | otherwise -> Right Interested
    3
      | BS.length msg /= 0 -> Left "Invalid message"
      | otherwise -> Right NotInterested
    4
      | BS.length msg /= 4 -> Left "Invalid message"
      | otherwise -> Right $ Have $ unpackInt msg
    5 -> Right $ BitField msg
    6
      | BS.length msg /= 12 -> Left "Invalid message"
      | otherwise ->
          Right $
            Request
              ( unpackInt $ slice 0 4 msg,
                unpackInt $ slice 4 4 msg,
                unpackInt $ slice 8 4 msg
              )
    7
      | BS.length msg < 8 -> Left "Invalid message"
      | otherwise ->
          Right $
            Piece
              ( unpackInt $ slice 0 4 msg,
                unpackInt $ slice 4 4 msg,
                slice 8 (BS.length msg - 8) msg
              )
    8
      | BS.length msg /= 12 -> Left "Invalid message"
      | otherwise ->
          Right $
            Cancel
              ( unpackInt $ slice 0 4 msg,
                unpackInt $ slice 4 4 msg,
                unpackInt $ slice 8 4 msg
              )
    13
      | BS.length msg /= 4 -> Left "Invalid message"
      | otherwise -> Right $ SuggestPiece $ unpackInt msg
    14
      | BS.length msg /= 0 -> Left "Invalid message"
      | otherwise -> Right HaveAll
    15
      | BS.length msg /= 0 -> Left "Invalid message"
      | otherwise -> Right HaveNone
    16
      | BS.length msg /= 12 -> Left "Invalid message"
      | otherwise ->
          Right $
            RejectRequest
              ( unpackInt $ slice 0 4 msg,
                unpackInt $ slice 4 4 msg,
                unpackInt $ slice 8 4 msg
              )
    17
      | BS.length msg /= 4 -> Left "Invalid message"
      | otherwise ->
          Right $ AllowedFast $ unpackInt $ slice 0 4 msg
    20
      | BS.length msg < 1 -> Left "Invalid message"
      | otherwise ->
          Right $
            Extended
              ( fromIntegral $ BS.head msg,
                BS.tail msg
              )
    21
      | BS.length msg /= 48 -> Left "Invalid message"
      | otherwise ->
          Right $
            HashRequest
              ( slice 0 32 msg,
                unpackInt $ slice 32 4 msg,
                unpackInt $ slice 36 4 msg,
                unpackInt $ slice 40 4 msg,
                unpackInt $ slice 44 4 msg
              )
    22
      | BS.length msg < 48 || (BS.length msg - 48) `mod` 32 /= 0 -> Left "Invalid message"
      | otherwise ->
          let hashes = (BS.length msg - 48) `div` 32
           in Right $
                Hashes
                  ( slice 0 32 msg,
                    unpackInt $ slice 32 4 msg,
                    unpackInt $ slice 36 4 msg,
                    unpackInt $ slice 40 4 msg,
                    unpackInt $ slice 44 4 msg,
                    [slice (48 + i * 32) 32 msg | i <- [0 .. hashes - 1]]
                  )
    23
      | BS.length msg /= 48 -> Left "Invalid message"
      | otherwise ->
          Right $
            HashReject
              ( slice 0 32 msg,
                unpackInt $ slice 32 4 msg,
                unpackInt $ slice 36 4 msg,
                unpackInt $ slice 40 4 msg,
                unpackInt $ slice 44 4 msg
              )
    _ -> Left "Invalid message"
