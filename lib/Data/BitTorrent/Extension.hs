{-# LANGUAGE OverloadedStrings #-}

module Data.BitTorrent.Extension where

import Data.BitTorrent.Bencode (BencodeValue (..), DecodeResult (DecodeResult))
import qualified Data.BitTorrent.Bencode as BE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map as Map

data Capability = Metadata deriving (Show, Eq)

encodeHandshake :: [Capability] -> ByteString
encodeHandshake capabilities =
  BE.encode $
    BDict
      ( Map.fromList
          [ ( "m",
              BDict
                ( Map.fromList
                    [ ("ut_metadata", if any ((==) Metadata) capabilities then BInt 1 else BInt 0)
                    ]
                )
            )
          ]
      )

decodeHandshake :: ByteString -> Maybe [(Capability, Int)]
decodeHandshake dat =
  case BE.decode dat of
    Right (BDict m) ->
      m Map.!? "m" >>= \rcap -> case rcap of
        BDict cap -> do
          let metadata =
                cap Map.!? "ut_metadata" >>= \rid -> case rid of
                  BInt c -> Just c
                  _ -> Nothing
          pure $ case metadata of
            Just n -> [(Metadata, fromInteger n)]
            Nothing -> []
        _ -> Nothing
    _ -> Nothing

data MetadataMessage = Request Int | Data Int ByteString | Reject Int deriving (Show, Eq)

data Message = UtMetadata MetadataMessage deriving (Show, Eq)

encode :: Message -> (Int, ByteString)
encode msg = case msg of
  UtMetadata (Request p) -> (1, BE.encode $ BDict $ Map.fromList [("msg_type", BInt 0), ("piece", BInt $ fromIntegral p)])
  UtMetadata (Data p b) ->
    ( 1,
      BS.concat
        [ BE.encode $
            BDict $
              Map.fromList
                [ ("msg_type", BInt 1),
                  ("piece", BInt $ fromIntegral p),
                  ("total_size", BInt $ fromIntegral $ BS.length b)
                ],
          b
        ]
    )
  UtMetadata (Reject p) -> (1, BE.encode $ BDict $ Map.fromList [("msg_type", BInt 2), ("piece", BInt $ fromIntegral p)])

decode :: ByteString -> Maybe Message
decode dat =
  case BE.decodeOne dat of
    Right (DecodeResult (BDict d) rd) ->
      case d Map.!? "msg_type" of
        Just (BInt i) -> case d Map.!? "piece" of
          Just (BInt p) -> case i of
            0 -> Just $ UtMetadata $ Request $ fromIntegral p
            1 -> case d Map.!? "total_size" of
              Just (BInt s) ->
                if BS.length dat /= rd + (fromIntegral s)
                  then Nothing
                  else
                    Just $ UtMetadata $ Data (fromIntegral p) $ BS.drop rd dat
              _ -> Nothing
            2 -> Just $ UtMetadata $ Reject $ fromIntegral p
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing
    _ -> Nothing
