module Main (main) where

import qualified Data.BitTorrent.Bencode as BE
import qualified Data.ByteString.Char8 as BS
import qualified Data.BitTorrent.Magnet as MG
import qualified Data.BitTorrent as TR
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), openFile)

main :: IO ()
main = do
  args <- getArgs
  print $ MG.decode $ head args
  -- file <- openFile (head args) ReadMode
  -- content <- BS.hGetContents file

  -- case BE.decode content of
  --   Left e -> putStrLn e
  --   Right d -> case TR.decode d of
  --     Left e -> putStrLn e
  --     Right v -> print v
