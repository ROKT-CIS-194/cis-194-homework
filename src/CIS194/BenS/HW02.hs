{-# OPTIONS_GHC -Wall #-}
module CIS194.BenS.HW02 where

import Data.List (unfoldr)

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = sum $ zipWith (\x y -> if x == y then 1 else 0) xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (`count` 0) colors
  where
    count = foldr (\p f q -> incr p q . f q) (const id) code
    incr p q = if p == q then (+1) else id

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches ps qs = sum (zipWith min (countColors ps) (countColors qs))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonexact
  where
    exact = exactMatches secret guess
    nonexact = matches secret guess - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact nonexact) code =
  exact == exact' && nonexact == nonexact'
  where
    exact' = exactMatches code guess
    nonexact' = matches code guess - exact'

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map return colors
allCodes n = do
  cs <- allCodes (n-1)
  c  <- colors
  return (c:cs)

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = unfoldr f (allCodes 4)
  where
    f [] = Nothing
    f (c:cs) = let m = getMove secret c in Just (m, filterCodes m cs)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
