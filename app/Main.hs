module Main (main) where

import qualified Data.Bencode as Be
import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), openFile)

main :: IO ()
main = do
  args <- getArgs
  file <- openFile (args !! 0) ReadMode
  content <- BS.hGetContents file

  putStrLn $ show $ Be.decode content

  pure ()
