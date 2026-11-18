module Main (main) where

import qualified Data.BitTorrent.Magnet as MG
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print $ MG.decode $ head args
