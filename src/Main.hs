module Main where

import CIS194.Testing
import qualified CIS194.Template.HW01Tests as HW01Tests
import Text.Printf

doTests :: String -> [Test] -> IO ()
doTests name tests = do
  let failures = runTests tests
  printf "%s: %d tests, %d failures\n" name (length tests) (length failures)
  mapM_ (printf "  %s\n" . show :: Failure -> IO ()) failures

main :: IO ()
main = do
  doTests "HW01" HW01Tests.allTests
