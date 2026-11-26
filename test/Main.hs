module Main (main) where

import qualified ArgsSpec
import qualified BencodeSpec
import qualified ExtensionSpec
import qualified MSESpec
import qualified MagnetSpec
import qualified PeerPoolSpec
import TestSupport (runTests)
import qualified TorrentSpec
import qualified TrackerHTTPSpec
import qualified TrackerUDPSpec
import qualified UTPSpec
import qualified WireSpec

main :: IO ()
main = runTests (ArgsSpec.tests ++ BencodeSpec.tests ++ ExtensionSpec.tests ++ MagnetSpec.tests ++ MSESpec.tests ++ PeerPoolSpec.tests ++ TorrentSpec.tests ++ WireSpec.tests ++ UTPSpec.tests ++ TrackerHTTPSpec.tests ++ TrackerUDPSpec.tests)
