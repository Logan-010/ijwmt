module Main (main) where

import qualified BencodeSpec
import qualified MagnetSpec
import TestSupport (runTests)
import qualified TorrentSpec
import qualified WireSpec

main :: IO ()
main = runTests (BencodeSpec.tests ++ MagnetSpec.tests ++ TorrentSpec.tests ++ WireSpec.tests)
