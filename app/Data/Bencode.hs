module Data.Bencode (decode, encode, BencodeValue (..)) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit)
import qualified Data.Map as Map
import Text.Read (readMaybe)

data BencodeValue
  = BString ByteString
  | BInt Integer
  | BList [BencodeValue]
  | BDict (Map.Map ByteString BencodeValue)
  deriving (Show, Eq)

parseString :: ByteString -> Either String (ByteString, ByteString)
parseString d =
  case BS.elemIndex ':' d of
    Nothing -> Left "Invalid encoded string"
    Just colonIndex ->
      case readMaybe (BS.unpack (BS.take colonIndex d)) :: Maybe Int of
        Nothing -> Left "Invalid string length"
        Just n ->
          let rest = BS.drop (colonIndex + 1) d
           in if n < 0 || n > BS.length rest
                then Left "Invalid string length"
                else Right (BS.take n rest, BS.drop n rest)

parseInt :: ByteString -> Either String Integer
parseInt d
  | BS.length d < 3 = Left "Invalid integer"
  | BS.head d /= 'i' || BS.last d /= 'e' = Left "Invalid integer"
  | otherwise =
      let digits = BS.unpack (BS.init (BS.tail d))
          validDigits = all (`elem` ['0' .. '9'])
          valid = case digits of
            "0" -> True
            '-' : v : rs -> v /= '0' && validDigits (v : rs)
            v : rs -> v /= '0' && validDigits (v : rs)
            _ -> False
       in if valid
            then case readMaybe digits of
              Just n -> Right n
              Nothing -> Left "Invalid integer"
            else Left "Invalid integer"

parseList :: ByteString -> Either String ([BencodeValue], ByteString)
parseList = inner []
  where
    inner acc s
      | BS.null s = Left "Unterminated list"
      | BS.head s == 'e' = Right (reverse acc, BS.tail s)
      | otherwise = do
          (value, rest) <- parseItem s
          inner (value : acc) rest

parseMap :: ByteString -> Either String (Map.Map ByteString BencodeValue, ByteString)
parseMap = inner Map.empty
  where
    inner acc s
      | BS.null s = Left "Unterminated map"
      | BS.head s == 'e' = Right (acc, BS.tail s)
      | otherwise = do
          (key, rest) <- parseString s
          (value, remaining) <- parseItem rest
          inner (Map.insert key value acc) remaining

parseItem :: ByteString -> Either String (BencodeValue, ByteString)
parseItem d
  | BS.null d = Left "Unexpected end of input"
  | isDigit (BS.head d) = do
      (value, rest) <- parseString d
      Right (BString value, rest)
  | otherwise =
      case BS.head d of
        'd' -> do
          (values, rest) <- parseMap (BS.tail d)
          Right (BDict values, rest)
        'l' -> do
          (values, rest) <- parseList (BS.tail d)
          Right (BList values, rest)
        'i' ->
          case BS.elemIndex 'e' d of
            Nothing -> Left "Invalid integer"
            Just endIndex -> do
              let len = endIndex + 1
              value <- parseInt (BS.take len d)
              Right (BInt value, BS.drop len d)
        _ -> Left "Invalid encoded value"

decode :: ByteString -> Either String BencodeValue
decode d = do
  (value, rest) <- parseItem d
  if BS.null rest
    then Right value
    else Left "Trailing data after encoded value"

encode :: BencodeValue -> ByteString
encode (BString s) =
  BS.pack (show (BS.length s)) `BS.append` BS.cons ':' s
encode (BInt n) =
  BS.pack ("i" ++ show n ++ "e")
encode (BList xs) =
  BS.concat [BS.singleton 'l', BS.concat (map encode xs), BS.singleton 'e']
encode (BDict m) =
  BS.concat
    [ BS.singleton 'd',
      BS.concat
        [ BS.append (encode (BString k)) (encode v)
        | (k, v) <- Map.toAscList m
        ],
      BS.singleton 'e'
    ]
