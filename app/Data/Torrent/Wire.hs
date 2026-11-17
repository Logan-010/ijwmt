module Data.Torrent.Wire (generatePeerId, handshake, Message (..), encodeMessage) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString, word32BE)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import System.Entropy (getEntropy)

generatePeerId :: IO ByteString
generatePeerId = getEntropy 20

handshake :: Bool -> ByteString -> ByteString -> ByteString
handshake v2 infoHash peerId
  | BS.length hash /= 20 = error "Info hash must be 20 bytes"
  | BS.length peerId /= 20 = error "Peer ID must be 20 bytes"
  | otherwise =
      BS.concat
        [ BS.singleton 19,
          BC.pack "BitTorrent protocol",
          BS.replicate 8 0,
          hash,
          peerId
        ]
  where
    hash
      | v2 = BS.take 20 infoHash
      | otherwise = infoHash

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
  | HashRequest (ByteString, Int, Int, Int, Int)
  | Hashes (ByteString, Int, Int, Int, Int, [ByteString])
  | HashReject (ByteString, Int, Int, Int, Int)
  deriving (Show, Eq)

packInt :: Int -> BS.ByteString
packInt n =
  BL.toStrict $
    toLazyByteString (word32BE (fromIntegral n))

packSingleInt :: Int -> BS.ByteString
packSingleInt n = BS.singleton (fromIntegral n)

{-
0	choke	None
1	unchoke	None
2	interested	None
3	not interested	None
4	have	uint32 piece_index
5	bitfield	Bit-packed piece availability
6	request	uint32 index, begin, length
7	piece	uint32 index, begin, bytes[]
8	cancel Same as request

21 hash_request
uint32  message_length = 49
uint8   message_id = 21
byte[32] pieces_root
uint32   base_layer
uint32   index
uint32   length
uint32   proof_layers

22 hashes
uint32  message_length = 49 + 32*N
uint8   message_id = 22
byte[32] pieces_root
uint32   base_layer
uint32   index
uint32   length
uint32   proof_layers
byte[32] hashes[N]

23 hash_reject
uint32  message_length = 49
uint8   message_id = 23
byte[32] pieces_root
uint32   base_layer
uint32   index
uint32   length
uint32   proof_layers
-}

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
