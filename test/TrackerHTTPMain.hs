module Main (main) where

import TestSupport (runTests)
import qualified TrackerHTTPSpec

main :: IO ()
main = runTests TrackerHTTPSpec.tests
