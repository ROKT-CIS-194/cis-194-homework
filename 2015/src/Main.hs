module Main where

import qualified CIS194.Template.HW01Tests       as HW01Tests
import qualified CIS194.Template.HW02Tests       as HW02Tests
import qualified CIS194.Template.HW03Tests       as HW03Tests
import           CIS194.Testing
import           Text.Printf

import           CIS194.Lectures.L01Intro
import           CIS194.Lectures.L02Lists
import           CIS194.Lectures.L03ADTs
import           CIS194.Lectures.L04TypeClasses
import           CIS194.Lectures.L05IO
import           CIS194.Lectures.L06Laziness     hiding (main)
import           CIS194.Lectures.L07Monads
import           CIS194.Lectures.L08MonadsII
import           CIS194.Lectures.L09Testing
import           CIS194.Lectures.L10TypeWizardry

doTests :: String -> [Test] -> IO ()
doTests name tests = do
  let failures = runTests tests
  printf "%s: %d tests, %d failures\n" name (length tests) (length failures)
  mapM_ (printf "  %s\n" . show :: Failure -> IO ()) failures

main :: IO ()
main = do
  doTests "HW01" HW01Tests.allTests
  doTests "HW02" HW02Tests.allTests
  doTests "HW03" HW03Tests.allTests
