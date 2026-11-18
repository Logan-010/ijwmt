module Main (main) where

import qualified BencodeSpec
import TestSupport (runTests)
import qualified TorrentSpec
import qualified WireSpec

main :: IO ()
main = runTests (BencodeSpec.tests ++ TorrentSpec.tests ++ WireSpec.tests)
