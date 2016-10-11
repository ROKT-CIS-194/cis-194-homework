{-# OPTIONS_GHC -Wall #-}
module CIS194.SamR.HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = mod x 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = floor (x / 10)

-- Exercise 2 -----------------------------------------

toRevDigitsHelper :: (Integer, [Integer]) -> (Integer, [Integer])
toRevDigitsHelper (n, xs)
  | n < 10    = (0, n : xs)
  | otherwise = toRevDigitsHelper (dropLastDigit n, (lastDigit n):xs)

toRevDigits :: Integer -> [Integer]
toRevDigits n = xs where (_, xs) = toRevDigitsHelper(n, [])

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = undefined

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = undefined


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = undefined

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
