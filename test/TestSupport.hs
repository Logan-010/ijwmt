module TestSupport (Test, assertEqual, assertLeft, assertThrows, runTests) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, unless)
import System.Exit (exitFailure)

type Test = (String, IO ())

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual =
  unless (expected == actual) $
    fail ("Expected " ++ show expected ++ ", got " ++ show actual)

assertLeft :: (Show a) => Either String a -> IO ()
assertLeft (Left _) = pure ()
assertLeft (Right value) = fail ("Expected rejection, got " ++ show value)

assertThrows :: a -> IO ()
assertThrows value = do
  result <- try (evaluate value >> pure ()) :: IO (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right () -> fail "Expected an exception"

runTests :: [Test] -> IO ()
runTests tests = do
  results <- forM tests $ \(name, action) -> do
    result <- try action :: IO (Either SomeException ())
    case result of
      Right () -> pure True
      Left err -> do
        putStrLn ("FAIL: " ++ name ++ "\n  " ++ show err)
        pure False
  let passed = length (filter id results)
  putStrLn (show passed ++ "/" ++ show (length tests) ++ " tests passed")
  unless (and results) exitFailure
