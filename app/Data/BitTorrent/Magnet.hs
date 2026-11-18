module Data.BitTorrent.Magnet (Topic (..), Magnet (..), decode) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base32 as B32
import qualified Data.ByteString.Char8 as BC
import Data.Char (toUpper)
import Data.List (stripPrefix)
import Data.Maybe (isNothing)
import Network.URL (URL, importURL, url_params)
import Text.Read (readMaybe)

data Topic
  = V1 ByteString
  | V2 ByteString
  deriving (Show, Eq)

data Magnet = Magnet
  { topic :: [Topic],
    displayName :: Maybe String,
    trackerUrl :: [URL],
    exactSize :: Maybe Integer,
    webSeed :: [URL],
    acceptableSource :: [URL],
    exactSource :: [URL],
    keywordTerms :: [String]
  }
  deriving (Show, Eq)

emptyMagnet :: Magnet
emptyMagnet = Magnet [] Nothing [] Nothing [] [] [] []

decodeHex :: String -> Int -> Either String ByteString
decodeHex hash expected = do
  bytes <- B16.decode (BC.pack hash)
  if BS.length bytes == expected
    then Right bytes
    else Left "Invalid hash length"

decodeV1 :: String -> Either String Topic
decodeV1 hash =
  case length hash of
    40 -> V1 <$> decodeHex hash 20
    32 -> do
      bytes <-
        first show $
          B32.decodeBase32Untyped $
            BC.pack (map toUpper hash)
      if BS.length bytes == 20
        then Right (V1 bytes)
        else Left "Invalid v1 hash length"
    _ -> Left "Invalid v1 hash length"

decodeV2 :: String -> Either String Topic
decodeV2 hash =
  case stripPrefix "1220" hash of
    Just digest -> V2 <$> decodeHex digest 32
    Nothing -> Left "Unsupported v2 multihash"

decodeTopic :: String -> Either String Topic
decodeTopic s
  | Just hash <- stripPrefix "urn:btih:" s =
      decodeV1 hash
  | Just hash <- stripPrefix "urn:btmh:" s =
      decodeV2 hash
  | otherwise =
      Left "Unsupported exact topic"

decode :: String -> Either String Magnet
decode d = case importURL d of
  Just u ->
    let decode' magnet params = case params of
          []
            | null $ topic magnet -> Left "No topic in magnet URL"
            | otherwise -> Right magnet
              { topic = reverse (topic magnet),
                trackerUrl = reverse (trackerUrl magnet),
                webSeed = reverse (webSeed magnet),
                acceptableSource = reverse (acceptableSource magnet),
                exactSource = reverse (exactSource magnet),
                keywordTerms = reverse (keywordTerms magnet)
              }
          (k, v) : t -> case k of
            "xt" -> decodeTopic v >>= \tp -> decode' (magnet {topic = tp : topic magnet}) t
            "dn"
              | isNothing $ displayName magnet -> decode' (magnet {displayName = Just v}) t
              | otherwise -> Left "Duplicate display name"
            "tr" -> case importURL v of
              Just tr -> decode' (magnet {trackerUrl = tr : trackerUrl magnet}) t
              Nothing -> Left "Invalid tracker URL"
            "xl"
              | isNothing $ exactSize magnet -> case readMaybe v of
                  Just s -> decode' (magnet {exactSize = Just s}) t
                  Nothing -> Left "Invalid size"
              | otherwise -> Left "Duplicate size"
            "ws" -> case importURL v of
              Just ws -> decode' (magnet {webSeed = ws : webSeed magnet}) t
              Nothing -> Left "Invalid web seed URL"
            "as" -> case importURL v of
              Just as -> decode' (magnet {acceptableSource = as : acceptableSource magnet}) t
              Nothing -> Left "Invalid acceptable source URL"
            "xs" -> case importURL v of
              Just xs -> decode' (magnet {exactSource = xs : exactSource magnet}) t
              Nothing -> Left "Invalid exact source URL"
            "kt" -> decode' (magnet {keywordTerms = v : keywordTerms magnet}) t
            _ -> Left "Invalid query paramater"
     in decode' emptyMagnet $ url_params u
  Nothing -> Left "Invalid URL"
