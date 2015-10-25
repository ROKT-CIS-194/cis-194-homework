--{-# OPTIONS_GHC -Wall #-}
module CIS194.ClaudioN.HW01 where

import Data.List

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n =  mod n 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
 | n <= 0    = []
 | otherwise = lastDigit n : toRevDigits (dropLastDigit n)


-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) = x : (2*y) : (doubleEveryOther zs)


-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = (foldl' (+) 0) . concat . (map toRevDigits)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = (0 ==) $ mod (sumDigits (doubleEveryOther (toRevDigits n))) 10


-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a _ c = [(a, c)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) b a c


-- Exercise 7 -----------------------------------------

-- Towers of Hanoi for *four* pegs

-- c.f. Frame-Stewart algorithm
-- https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame.E2.80.93Stewart_algorithm
kOpt4 :: Integer -> Integer
kOpt4 = (subtract 1) . toInteger . round . sqrt . fromInteger . (1 +) . (2 *)

-- turns out wikipedia formula for k above is not optimal
--  * for example: kOpt4 3 => 2 (when it should clearly be 1)
-- Using alternative: https://www2.bc.edu/~grigsbyj/Rand_Final.pdf
lOpt4 :: Integer -> Integer
lOpt4 n = n - toInteger(floor(sqrt(2 * fromInteger(n)) + 0.5))

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a _ _ d = [(a, d)]
-- 1) Move top k to a single peg (b/c), other than start (a) or dest (d)
-- 2) Move remaining n-k disks to dest (d), without using peg in 1
-- 3) Move top k disks to dest (d)
hanoi4 n a b c d =
--  let k = lOpt4 n
--  in hanoi4 k a ? ? b ++ hanoi (n-k) a c d ++ hanoi4 k b ? ? d
  let k = lOpt4 n
  in hanoi4 k a d c b ++ hanoi (n-k) a c d ++ hanoi4 k b a c d
