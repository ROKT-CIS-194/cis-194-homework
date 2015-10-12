--{-# OPTIONS_GHC -Wall #-}
module CIS194.ClaudioN.Lesson01 where

x :: Int
x = 3

-- x = 4

y :: Int
y = y + 1


-- Machine-sized integers
i :: Int
i = -78

biggestInt, smallestInt :: Int
biggestInt  = maxBound
smallestInt = minBound


-- Arbitrary-precision integers
n :: Integer
n = 1234567890987654321987340982334987349872349874534

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

numDigits :: Int
numDigits = length (show reallyBig)

-- Booleans
b1, b2 :: Bool
b1 = True
b2 = False

-- Unicode characters
c1, c2, c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'

-- Strings are lists of characters with special syntax
s :: String
s = "Hello, Haskell!"


-- Exercises
ex01 = 3 + 2
ex02 = 19 - 27
ex03 = 2.35 * 8.6
ex04 = 8.7 / 3.1
ex05 = mod 19 3
ex06 = 19 `mod` 3
ex07 = 7 ^ 222
ex08 = (-3) * (-7)
ex09 = i `div` i
ex10 = 12 `div` 5
ex11 = True && False
ex12 = not (False || True)
ex13 = ('a' == 'a')
ex14 = (16 /= 3)
ex15 = (5 > 3) && ('p' <= 'q')
ex16 = "Haskell" > "C++"


-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)


-- Guards
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1


foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3


isEven :: Integer -> Bool
isEven n
  | n `mod` 2 == 0 = True
  | otherwise      = False


-- Pairs
p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int,Int) -> Int
sumPair (x,y) = x + y


-- Multiple args
f :: Int -> Int -> Int -> Int
f x y z = x + y + z
ex17 = f 3 17 8


-- Lists
nums, range, range2 :: [Integer]
nums   = [1,2,3,19]
range  = [1..100]
range2 = [2,4..100]


-- hello1 and hello2 are exactly the same.

hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2


-- Constructing Lists
emptyList = []

ex18 = 1 : []
ex19 = 3 : (1 : [])
ex20 = 2 : 3 : 4 : []
ex21 = [2,3,4] == 2 : 3 : 4 : []


-- Generate the sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)


-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

-- Nested patterns
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs


-- The number of hailstone steps needed to reach 1 from a starting
-- number.
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
