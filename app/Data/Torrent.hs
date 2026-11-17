{-# LANGUAGE DuplicateRecordFields #-}

module Data.Torrent where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)

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
