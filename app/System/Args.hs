module System.Args (Args (..), parse, helpStr, versionStr) where

import Data.BitTorrent (Torrent)
import qualified Data.BitTorrent as TR
import qualified Data.BitTorrent.Bencode as BE
import Data.BitTorrent.Magnet (Magnet)
import qualified Data.BitTorrent.Magnet as MG
import qualified Data.ByteString as BS
import Data.List (uncons)

data Args = Args
  { help :: Bool,
    version :: Bool,
    noTcp :: Bool,
    tcpAddr :: String,
    noUtp :: Bool,
    utpAddr :: String,
    files :: [Torrent],
    magnets :: [Magnet]
  }
  deriving (Show, Eq)

defaultArgs :: Args
defaultArgs = Args False False False "[::]:8877" False "[::]:8877" [] []

helpStr :: String
helpStr =
  unlines
    [ "Usage: ijwmt [OPTIONS] [MAGNET ...]",
      "",
      "I just want my torrent - purely functional torrenting",
      "",
      "Options:",
      "  -h, --help            Show this help message",
      "  -v, --version         Show version information",
      "  -t, --noTcp           Disable TCP connections",
      "  -T, --tcpAddr ADDR    Set TCP listen address (default: [::]:8877)",
      "  -u, --noUtp           Disable uTP connections",
      "  -U, --utpAddr ADDR    Set uTP listen address (default: [::]:8877)",
      "  -f, --file PATH       Load a torrent file (can be repeated)",
      "",
      "Arguments:",
      "  MAGNET                Magnet URI to download (can be repeated)",
      "",
      "Examples:",
      "  ijwmt magnet:?xt=urn:btih:...",
      "  ijwmt -f example.torrent",
      "  ijwmt -T 0.0.0.0:6881 -f example.torrent",
      "  ijwmt -t -f example.torrent"
    ]

versionStr :: String
versionStr = "ijwmt 0.1.0.0"

parse :: [String] -> IO (Either String Args)
parse args =
  parse' args defaultArgs
  where
    parse' a p = case a of
      [] -> pure $ Right p
      h : t
        | h `elem` ["-h", "--help"] -> parse' t $ p {help = True}
        | h `elem` ["-v", "--version"] -> parse' t $ p {version = True}
        | h `elem` ["-t", "--noTcp"] -> parse' t $ p {noTcp = True}
        | h `elem` ["-T", "--tcpAddr"] -> case uncons t of
            Just (he, ta) -> parse' ta $ p {tcpAddr = he}
            Nothing -> pure $ Left "Missing TCP listen address"
        | h `elem` ["-u", "--noUtp"] -> parse' t $ p {noUtp = True}
        | h `elem` ["-U", "--utpAddr"] -> case uncons t of
            Just (he, ta) -> parse' ta $ p {utpAddr = he}
            Nothing -> pure $ Left "Missing uTP listen address"
        | h `elem` ["-f", "--file"] -> case uncons t of
            Just (he, ta) ->
              BS.readFile he >>= \c ->
                case BE.decode c >>= TR.decode of
                  Left err -> pure $ Left err
                  Right to -> parse' ta $ p {files = to : files p}
            Nothing -> pure $ Left "Missing file path"
        | otherwise ->
            case MG.decode h of
              Left err -> pure $ Left err
              Right mg -> parse' t $ p {magnets = mg : magnets p}
