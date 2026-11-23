module Main (main) where

import qualified BencodeSpec
import qualified MSESpec
import qualified MagnetSpec
import TestSupport (runTests)
import qualified TorrentSpec
import qualified TrackerHTTPSpec
import qualified UTPSpec
import qualified WireSpec

main :: IO ()
main = runTests (BencodeSpec.tests ++ MagnetSpec.tests ++ MSESpec.tests ++ TorrentSpec.tests ++ TrackerHTTPSpec.tests ++ WireSpec.tests ++ UTPSpec.tests)
