-- CIS 194, Spring 2015
--
-- Test cases for HW 02

module CIS194.ClaudioN.HW02Tests where

import CIS194.ClaudioN.HW02
import CIS194.Testing


-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF2 "exactMatches test" exactMatches
             [ ([Red, Blue, Green, Yellow], [Blue, Green, Yellow, Red], 0)
             , ([Red, Blue, Green, Yellow], [Red, Purple, Green, Orange], 2)
             ]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF1 "countColors test" countColors
             [ ([Red, Blue, Yellow, Purple], [1, 0, 1, 1, 0, 1])
             , ([Green, Blue, Green, Orange], [0, 2, 1, 0, 1, 0])
             ]
           , testF2 "matches test" matches
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue], 3) ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF2 "getMove test" getMove
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue],
               Move [Red, Orange, Orange, Blue] 1 2)
             ]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF2 "isConsistent test" isConsistent
             [ (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Yellow, Purple],
               True)
             , (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Red, Purple],
               False)
             ]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ testF2 "filterCodes tests" ((length .) . filterCodes)
             [ (Move [Red, Red, Red, Red] 4 0, (allCodes 4),   1)
             , (Move [Red, Red, Red, Red] 3 0, (allCodes 4),  20)
             , (Move [Red, Red, Red, Red] 2 0, (allCodes 4), 150)
             , (Move [Red, Red, Red, Red] 1 0, (allCodes 4), 500)
             , (Move [Red, Red, Red, Red] 0 0, (allCodes 4), 625)
             , (Move [Red, Red, Red, Red] 0 1, (allCodes 4), 0)
             , (Move [Red, Red, Red, Blue] 2 1, (allCodes 4), 24)
             ]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [testF1 "allCodes test" ((take 7) . allCodes)
             [ (0, [[]])
             , (1, [[Red],[Green],[Blue],[Yellow],[Orange],[Purple]])
             , (2, [[Red,Red],[Red,Green],[Red,Blue],[Red,Yellow],[Red,Orange],[Red,Purple],[Green,Red]])
             ]
           ,testF1 "allCodes test" (length . allCodes)
             [ (0, 1), (1, 6), (2, 36), (3, 216), (4, 1296), (5, 7776) ]
           ]

-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests = []

-- Bonus ----------------------------------------------

bonusTests :: [Test]
bonusTests = [testF1 "fiveGuess" (sequence [length, maximum, minimum, sum] . map (length . fiveGuess) . ($ allCodes 4) . take)
               [ (  10, [  10, 5, 1,    37]) -- for the first few codes, should include min of 1 (firstGuess)
                                             -- and a max of 5 (and an arbitrary sum of 37)
--               , (1296, [1296, 5, 1, 5801])  -- sum of 5801 as per http://arxiv.org/pdf/1305.1010.pdf,
                                             -- but waaaaay too slow!
               ]
             ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , bonusTests
                  ]
