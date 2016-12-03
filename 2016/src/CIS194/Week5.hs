{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week5 where

-- EXERCISE 1: LISTS, LISTS, LISTS

-- | Halve Evens -- from a list of integers, remove any odd entry and halve
-- every even entry.
halveEvens :: [Integer] -> [Integer]
halveEvens = undefined

ex_halveEvens :: [Bool]
ex_halveEvens =
    [ halveEvens [] == []
    , halveEvens [1,2,3,4,5] == [1,2]
    , halveEvens [6,6,6,3,3,3,2,2,2] == [3,3,3,1,1,1]
    ]

-- | Safe String - in a string, replace every character that is a control
-- character or not an ASCII character by an underscore. Use the Data.Char
-- module.
safeString :: String -> String
safeString = undefined

ex_safeString :: [Bool]
ex_safeString =
    [ safeString [] == []
    , safeString "Hello World!" == "Hello World!"
    , safeString "That’s your line:\n" == "That_s your line:_"
    , safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
    ]

-- | Holes - given a list, return the a list of lists that contains every list
-- that is obtained by the original list by removing one element, in order. (The
-- examples might be more helpful).
holes :: [a] -> [[a]]
holes = undefined

ex_holes :: [Bool]
ex_holes =
   [ holes "" == []
   , holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
   ]

-- | Longest Text - given a non-empty list, find the entry for which show
-- results the longest text shown.  If there are ties, prefer the last one.
longestText :: Show a => [a] -> a
longestText = undefined

ex_longestText :: [Bool]
ex_longestText =
   [ longestText [True,False] == False
   , longestText [2,4,16,32] == (32::Int)
   , longestText (words "Hello World") == "World"
   , longestText (words "Olá mundo") ==  "Olá"
   ]

-- | Adjacents - pair each element with the next one in the list.
adjacents :: [a] -> [(a,a)]
adjacents = undefined

ex_adjacents :: [Bool]
ex_adjacents =
   [ adjacents "" == []
   , adjacents [True] == []
   , adjacents "Hello" == [('H','e'),('e','l'),('l','l'),('l','o')]
   ]

-- | Commas - add commas between strings.
commas :: [String] -> String
commas = undefined

ex_commas :: [Bool]
ex_commas =
   [ commas [] == ""
   , commas ["Hello"] == "Hello"
   , commas ["Hello", "World"] == "Hello, World"
   , commas ["Hello", "", "World"] == "Hello, , World"
   , commas ["Hello", "new", "World"] == "Hello, new, World"
   ]

-- | Add Polynomials - Given coefficients to polynomial equations as lists of
-- the same length, output the coefficients for the sum of these equations.
--
-- You may assume that at least one polynomial is given.
addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = undefined

ex_addPolynomials :: [Bool]
ex_addPolynomials =
   [ addPolynomials [[]] == []
   , addPolynomials [[0, 1], [1, 1]] == [1, 2]
   , addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
   ]

-- | Sum Numbers - Output the sum of all natural numbers contained in the given
-- string. A natural number in this sense is any maximal subsequence of digits,
-- i.e. one that is neither preceded nor followed by an integer. (The examples
-- should provide more clarification.)
sumNumbers :: String -> Integer
sumNumbers = undefined

ex_sumNumbers :: [Bool]
ex_sumNumbers =
   [ sumNumbers "" == 0
   , sumNumbers "Hello world!" == 0
   , sumNumbers "a1bc222d3f44" == 270
   , sumNumbers "words0are1234separated12by3integers45678" == 46927
   , sumNumbers "000a." == 0
   , sumNumbers "0.00a." == 0
   ]

-- EXERCISE 2: WORD COUNT

wordCount :: String -> String
wordCount = undefined

-- EXERCISE 3

testResults :: [(String, [Bool])]
testResults = []

formatTests :: [(String, [Bool])] -> String
formatTests = undefined

-- MAIN

main :: IO ()
main = do
  undefined
