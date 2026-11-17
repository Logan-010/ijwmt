module Main (main) where

import qualified Data.Bencode as Be
import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \a -> putStrLn $ show $ Be.decode $ BS.pack $ a !! 0
