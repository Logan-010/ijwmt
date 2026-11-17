module Main (main) where

import qualified Data.Bencode as BE
import qualified Data.ByteString.Char8 as BS
import qualified Data.Torrent as TR
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), openFile)

main :: IO ()
main = do
  args <- getArgs
  file <- openFile (args !! 0) ReadMode
  content <- BS.hGetContents file

  case BE.decode content of
    Left e -> putStrLn e
    Right d -> case TR.decode d of
      Left e -> putStrLn e
      Right v -> putStrLn $ show v
