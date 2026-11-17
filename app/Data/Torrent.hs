{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Torrent(decode, Torrent(..), FileAttributes(..), TorrentFile(..), TorrentV1(..), TorrentV2(..), TorrentVersion) where

import Control.Monad (unless, when)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bencode (BencodeValue (..), encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data TorrentFile = TorrentFile
  { path :: [Text],
    length :: Integer,
    attributes :: FileAttributes,
    symlinkPath :: Maybe [Text],
    sha1 :: Maybe ByteString,
    piecesRoot :: Maybe ByteString
  }
  deriving (Show, Eq)

data FileAttributes = FileAttributes
  { padding :: Bool,
    executable :: Bool,
    hidden :: Bool,
    symlink :: Bool
  }
  deriving (Show, Eq)

data TorrentV1 = TorrentV1
  { infoHash :: ByteString,
    pieceHashes :: [ByteString],
    files :: [TorrentFile],
    singleFile :: Bool
  }
  deriving (Show, Eq)

data TorrentV2 = TorrentV2
  { infoHash :: ByteString,
    files :: [TorrentFile],
    pieceLayers :: Map ByteString [ByteString]
  }
  deriving (Show, Eq)

data TorrentVersion
  = V1 TorrentV1
  | V2 TorrentV2
  | Hybrid TorrentV1 TorrentV2
  deriving (Show, Eq)

data Torrent = Torrent
  { name :: Text,
    announce :: Maybe Text,
    announceList :: [[Text]],
    webSeeds :: [Text],
    comment :: Maybe Text,
    createdBy :: Maybe Text,
    creationDate :: Maybe Integer,
    pieceLength :: Integer,
    private :: Bool,
    source :: Maybe Text,
    version :: TorrentVersion
  }
  deriving (Show, Eq)

decode :: BencodeValue -> Either String Torrent
decode d = do
  top <- dictionary d
  infoValue <- required "info" top
  info <- dictionary infoValue
  torrentName <- required "name" info >>= text
  validateComponent torrentName
  tracker <- optional "announce" text top
  trackers <- maybe (Right []) (listOf (listOf text)) (Map.lookup "announce-list" top)
  seeds <- maybe (Right []) seedList (Map.lookup "url-list" top)
  torrentComment <- optional "comment" text top
  creator <- optional "created by" text top
  created <- optional "creation date" integer top
  size <- required "piece length" info >>= integer
  unless (size > 0) (Left "piece length must be positive")
  isPrivate <- maybe (Right False) privateFlag (Map.lookup "private" info)
  torrentSource <- optional "source" text info
  metaVersion <- optional "meta version" integer info
  let encodedInfo = encode infoValue
      v1 = parseV1 torrentName size encodedInfo info
      v2 = parseV2 size encodedInfo top info
  torrentVersion <- case metaVersion of
    Nothing -> V1 <$> v1
    Just 2 -> do
      torrentV2 <- v2
      case Map.lookup "pieces" info of
        Nothing -> Right (V2 torrentV2)
        Just _ -> Hybrid <$> v1 <*> pure torrentV2
    Just _ -> Left "Unsupported meta version"
  pure Torrent
    { name = torrentName,
      announce = tracker,
      announceList = trackers,
      webSeeds = seeds,
      comment = torrentComment,
      createdBy = creator,
      creationDate = created,
      pieceLength = size,
      private = isPrivate,
      source = torrentSource,
      version = torrentVersion
    }
  where
    dictionary (BDict values) = Right values
    dictionary _ = Left "Expected a dictionary"

    bytes (BString value) = Right value
    bytes _ = Left "Expected a byte string"

    integer (BInt value) = Right value
    integer _ = Left "Expected an integer"

    text value = do
      raw <- bytes value
      case Text.decodeUtf8' raw of
        Left _ -> Left "Invalid UTF-8 text"
        Right decoded -> Right decoded

    listOf parse (BList values) = traverse parse values
    listOf _ _ = Left "Expected a list"

    required key values =
      maybe (Left ("Missing field: " ++ show key)) Right (Map.lookup key values)

    optional key parse values = traverse parse (Map.lookup key values)

    seedList value@(BString _) = (: []) <$> text value
    seedList value = listOf text value

    privateFlag (BInt 0) = Right False
    privateFlag (BInt 1) = Right True
    privateFlag _ = Left "private must be 0 or 1"

    validateComponent component =
      when (Text.null component || component == "." || component == ".."
        || Text.any (`elem` ['/', '\\', '\0']) component)
        (Left "Invalid file path component")

    filePath value = do
      components <- listOf text value
      when (null components) (Left "File path must not be empty")
      mapM_ validateComponent components
      pure components

    chunks width raw
      | BS.null raw = []
      | otherwise = BS.take width raw : chunks width (BS.drop width raw)

    hashes width value = do
      raw <- bytes value
      unless (BS.length raw `mod` width == 0) (Left "Invalid hash string length")
      pure (chunks width raw)

    hashBytes width value = do
      raw <- bytes value
      unless (BS.length raw == width) (Left "Invalid hash length")
      pure raw

    parseFile components values = do
      attrs <- maybe (Right BS.empty) bytes (Map.lookup "attr" values)
      let flags = FileAttributes
            { padding = BS.elem 112 attrs,
              executable = BS.elem 120 attrs,
              hidden = BS.elem 104 attrs,
              symlink = BS.elem 108 attrs
            }
      fileLength <- case Map.lookup "length" values of
        Nothing | symlink flags -> Right 0
        _ -> required "length" values >>= integer
      when (fileLength < 0) (Left "File length must not be negative")
      link <- optional "symlink path" filePath values
      when (symlink flags && link == Nothing) (Left "Missing symlink path")
      checksum <- optional "sha1" (hashBytes 20) values
      root <- optional "pieces root" (hashBytes 32) values
      pure TorrentFile
        { path = components,
          length = fileLength,
          attributes = flags,
          symlinkPath = link,
          sha1 = checksum,
          piecesRoot = root
        }

    parseV1 torrentName size encodedInfo info = do
      pieceHashes' <- required "pieces" info >>= hashes 20
      (torrentFiles, isSingle) <- case (Map.lookup "length" info, Map.lookup "files" info) of
        (Just _, Nothing) -> do
          file <- parseFile [torrentName] info
          pure ([file], True)
        (Nothing, Just value) -> do
          torrentFiles <- listOf (\entry -> do
            values <- dictionary entry
            components <- required "path" values >>= filePath
            parseFile components values) value
          when (null torrentFiles) (Left "files must not be empty")
          pure (torrentFiles, False)
        _ -> Left "Expected exactly one of length or files"
      let total = sum [n | TorrentFile {length = n} <- torrentFiles]
          count = (total + size - 1) `div` size
      unless (toInteger (Prelude.length pieceHashes') == count)
        (Left "Piece hash count does not match file lengths")
      pure TorrentV1
        { infoHash = SHA1.hash encodedInfo,
          pieceHashes = pieceHashes',
          files = torrentFiles,
          singleFile = isSingle
        }

    parseV2 size encodedInfo top info = do
      unless (size >= 16384 && powerOfTwo size)
        (Left "v2 piece length must be a power of two and at least 16384")
      tree <- required "file tree" info >>= dictionary
      torrentFiles <- walkTree [] tree
      when (null torrentFiles) (Left "File tree must not be empty")
      layers <- maybe (Right Map.empty) parseLayers (Map.lookup "piece layers" top)
      mapM_ (validateLayer size layers) torrentFiles
      pure TorrentV2
        { infoHash = SHA256.hash encodedInfo,
          files = torrentFiles,
          pieceLayers = layers
        }

    powerOfTwo n
      | n == 1 = True
      | odd n || n <= 0 = False
      | otherwise = powerOfTwo (n `div` 2)

    walkTree components tree = case Map.lookup "" tree of
      Just leaf -> do
        when (null components || Map.size tree /= 1)
          (Left "Invalid file tree leaf")
        values <- dictionary leaf
        file <- parseFile components values
        case file of
          TorrentFile {length = n, piecesRoot = root, attributes = flags} ->
            when (n > 0 && not (symlink flags) && root == Nothing)
              (Left "Missing pieces root")
        pure [file]
      Nothing -> concat <$> traverse (\(key, value) -> do
        component <- text (BString key)
        validateComponent component
        children <- dictionary value
        when (Map.null children) (Left "Empty file tree directory")
        walkTree (components ++ [component]) children) (Map.toAscList tree)

    parseLayers value = do
      layers <- dictionary value
      Map.fromList <$> traverse (\(root, layer) -> do
        unless (BS.length root == 32) (Left "Invalid piece layer root")
        layerHashes <- hashes 32 layer
        pure (root, layerHashes)) (Map.toAscList layers)

    validateLayer size layers TorrentFile {length = n, piecesRoot = root, attributes = flags}
      | n <= size || symlink flags = Right ()
      | otherwise = do
          layer <- maybe (Left "Missing piece layer") Right (root >>= (`Map.lookup` layers))
          unless (toInteger (Prelude.length layer) == (n + size - 1) `div` size)
            (Left "Piece layer hash count does not match file length")
