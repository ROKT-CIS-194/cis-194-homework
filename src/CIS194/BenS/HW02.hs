{-# OPTIONS_GHC -Wall #-}
module CIS194.BenS.HW02 where

import Data.Bool

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
exactMatches xs ys = sum . map (bool 0 1) $ zipWith (==) xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors = foldr (zipWith (+)) [0, 0, 0, 0, 0, 0] . map code
  where
    code Red    = [1, 0, 0, 0, 0, 0]
    code Green  = [0, 1, 0, 0, 0, 0]
    code Blue   = [0, 0, 1, 0, 0, 0]
    code Yellow = [0, 0, 0, 1, 0, 0]
    code Orange = [0, 0, 0, 0, 1, 0]
    code Purple = [0, 0, 0, 0, 0, 1]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches secret guess =
  sum (zipWith min (countColors secret) (countColors guess))

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
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = map (:[]) colors
allCodes n = concatMap (\cs -> map (\c -> c:cs) colors) (allCodes (n-1))

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = reverse $ go [] (allCodes 4)
  where
    go ms [] = ms
    go ms (c:cs) = go (m:ms) (filterCodes m cs) where m = getMove secret c

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
