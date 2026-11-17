module Data.Bencode where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit)
import qualified Data.Map as Map
import Text.Read (readMaybe)

data BencodeValue
  = BString String
  | BInt Integer
  | BList [BencodeValue]
  | BDict (Map.Map String BencodeValue)
  deriving (Show, Eq)

decode :: ByteString -> Either String BencodeValue
decode d
  | isDigit $ BS.head d = case BS.elemIndex ':' d of
      Just colonIndex -> case (readMaybe . BS.unpack . BS.take colonIndex) d of
        Just l ->
          let ss = BS.drop (colonIndex + 1) d
           in if BS.length ss /= l then Left "Invalid string length" else Right (BString (BS.unpack ss))
        Nothing -> Left "Invalid encoded value"
      Nothing -> Left "Invalid encoded value"
  | otherwise = case BS.head d of
      'd' -> Left "todo"
      'l' -> Left "todo"
      'i' -> case BS.elemIndex 'e' d of
        Just i -> case (readMaybe . BS.unpack . BS.drop 1 . BS.take i) d of
          Just n -> Right (BInt n)
          Nothing -> Left "Invalid integer"
        Nothing -> Left "Invalid integer format"
      _ -> Left "Invalid encoded value"
