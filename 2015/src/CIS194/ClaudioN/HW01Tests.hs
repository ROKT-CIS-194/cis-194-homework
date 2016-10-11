-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module CIS194.ClaudioN.HW01Tests where

import CIS194.ClaudioN.HW01
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
testToRevDigits (n, l) = toRevDigits n == l

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits" testToRevDigits
             [(1234, [4, 3, 2, 1]), (0, []), (-17, [])]]

-- Exercise 3 -----------------------------------------
testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (l1, l2) = doubleEveryOther l1 == l2

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther" testDoubleEveryOther
             [([4, 9, 5, 5], [4, 18, 5, 10]), ([0, 0], [0, 0])]]

-- Exercise 4 -----------------------------------------
testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits(l, n) = sumDigits l == n

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits" testSumDigits
             [([4, 9, 5, 5], 23), ([0, 0], 0), ([10, 5, 18, 4], 19)]]

-- Exercise 5 -----------------------------------------
testLuhn :: (Integer, Bool) -> Bool
testLuhn(n, b) = luhn n == b

ex5Tests :: [Test]
ex5Tests = [ Test "luhn" testLuhn
             [(5594589764218858, True), (1234567898765432, False)]]

-- Exercise 6 -----------------------------------------
testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi(n, p1, p2, p3, m) = hanoi n p1 p2 p3 == m

testHanoiCount :: (Integer, Int) -> Bool
testHanoiCount(n, c) = length(hanoi n "a" "b" "c") == c

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi moves" testHanoi
             [(2, "a", "b", "c", [("a","b"), ("a","c"), ("b","c")]),
              (3, "a", "b", "c", [("a","c"), ("a","b"), ("c","b"), ("a","c"), ("b","a"), ("b","c"), ("a","c")])]]

ex6TestsCount :: [Test]
ex6TestsCount = [ Test "hanoi count" testHanoiCount
             [( 1,  1),
              ( 2,  3),
              ( 3,  7),
              ( 4, 15),
              (15, 32767)]]


-- Exercise 7 -----------------------------------------
testHanoi4 :: (Integer, Peg, Peg, Peg, Peg, [Move]) -> Bool
testHanoi4(n, p1, p2, p3, p4, m) = hanoi4 n p1 p2 p3 p4 == m

testHanoi4Count :: (Integer, Int) -> Bool
testHanoi4Count(n, c) = length(hanoi4 n "a" "b" "c" "d") == c

ex7Tests :: [Test]
ex7Tests = [ Test "hanoi4 moves" testHanoi4
             [(2, "a", "b", "c", "d", [("a","c"),("a","d"),("c","d")]),
              (3, "a", "b", "c", "d", [("a","b"),("a","c"),("a","d"),("c","d"),("b","d")]),
              (4, "a", "b", "c", "d", [("a","b"),("a","d"),("a","c"),("d","c"),("a","d"),("c","a"),("c","d"),("a","d"),("b","d")])]]

ex7TestsCount :: [Test]
ex7TestsCount = [ Test "hanoi4 count" testHanoi4Count
             [( 1,  1),
              ( 2,  3),
              ( 3,  5),
              ( 4,  9),
              ( 5, 13),
              ( 6, 17),
              (15, 129)]]


-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex6TestsCount
                  , ex7Tests
                  , ex7TestsCount
                  ]
