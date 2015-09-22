module Main where

import CIS194.Testing
import qualified CIS194.Template.HW01Tests as HW01Tests

main :: IO ()
main = do
  mapM_ print (runTests HW01Tests.allTests)
