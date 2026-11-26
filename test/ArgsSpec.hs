module ArgsSpec (tests) where

import qualified System.Args as AG
import TestSupport (Test, assertEqual, assertLeft)

tests :: [Test]
tests =
  [ ("CLI shared listener defaults", do
        args <- parseArgs []
        assertEqual "::" (AG.interface args)
        assertEqual 8877 (AG.port args)),
    ("CLI shared listener short flags", do
        args <- parseArgs ["-i", "0.0.0.0", "-p", "6881"]
        assertEqual "0.0.0.0" (AG.interface args)
        assertEqual 6881 (AG.port args)),
    ("CLI shared listener long flags and protocol switches", do
        args <- parseArgs ["--interface", "::1", "--port", "65535", "--noTcp", "--noUtp"]
        assertEqual "::1" (AG.interface args)
        assertEqual 65535 (AG.port args)
        assertEqual True (AG.noTcp args)
        assertEqual True (AG.noUtp args)),
    ("CLI automatic listener port", do
        args <- parseArgs ["--port", "0"]
        assertEqual 0 (AG.port args)),
    ("CLI rejects missing listener arguments",
        mapM_ (\args -> AG.parse args >>= assertLeft) [["-i"], ["--interface"], ["-p"], ["--port"]]),
    ("CLI rejects invalid ports",
        mapM_ (\port -> AG.parse ["--port", port] >>= assertLeft) ["-1", "65536", "abc", "6881.0"])
  ]

parseArgs :: [String] -> IO AG.Args
parseArgs args = AG.parse args >>= either fail pure
