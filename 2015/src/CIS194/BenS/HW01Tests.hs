-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module CIS194.BenS.HW01Tests where

import CIS194.BenS.HW01
import CIS194.Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, ds) = toRevDigits n == ds

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits" testToRevDigits
             [ (123, [3,2,1]), (1234, [4,3,2,1]), (5, [5])
             , (10, [0,1]), (0, []), (-17, [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (ns, ms) = doubleEveryOther ns == ms

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther" testDoubleEveryOther
             [ ([4, 9, 5, 5], [4, 18, 5, 10])
             , ([1, 2, 3, 4], [1, 4, 3, 8])
             , ([0, 0], [0, 0])
             ]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (ns, d) = sumDigits ns == d

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits" testSumDigits
             [ ([10, 5, 18, 4], 19)
             , ([12, 34], 10)
             , ([], 0)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, b) = luhn n == b

ex5Tests :: [Test]
ex5Tests = [ Test "luhn" testLuhn
             [ (5519574098150870, True)
             , (5338434647685976, True)
             , (5259808266938862, True)
             , (5584850814102498, True)
             , (5546675904618187, True)
             , (5546675904618186, False)
             ]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
