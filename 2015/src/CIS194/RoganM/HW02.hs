{-# OPTIONS_GHC -Wall #-}
module CIS194.Template.HW02 where

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
exactMatches actual guess = length (filter id (zipWith (==) actual guess))

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (length . filter id) [[(c == p) | p <- code] | c <- colors]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum (zipWith min (countColors actual) (countColors guess))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess (exactMatches secret guess) (matches secret guess)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact match) code =
  (exactMatches code guess) == exact && (matches code guess) == match

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes mv codes = filter (isConsistent mv) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes k =
  allCodes' k []
  where allCodes' 0 codes = [codes]
        allCodes' n codes = concatMap (allCodes' (n - 1) . (: codes)) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = solve' secret [getMove secret (take (length secret) (repeat Red))] (allCodes)
  where solve' s m a =
          if s == (fst m) -- how do I unpack this frigging thing
             then m
                  -- it looks like Lisp hehehe
             else solve' s ((getMove secret (fst (filterCodes (fst m) a))) : m) (filterCodes (fst m) a)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
