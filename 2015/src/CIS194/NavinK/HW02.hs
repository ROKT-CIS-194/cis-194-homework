{-# OPTIONS_GHC -Wall #-}
module CIS194.NavinK.HW02 where

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
exactMatches _ [] = 0
exactMatches [] _ = 0
exactMatches (x:xs) (y:ys)
  | x == y = 1 + exactMatches xs ys
  | otherwise = exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors = (\y -> map (\x -> length $ filter (== x) y) colors)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches code1 code2 = sum $ [min x y | (x,y) <- zip a b]
  where
    a = countColors(code1)
    b = countColors(code2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exacts nonExacts
  where
    exacts = exactMatches secret guess
    nonExacts = matches secret guess - exacts

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move code = move == getMove code guess
  where
    Move guess _ _ = move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
  | n == 1 = [[Red],[Green],[Blue],[Yellow],[Orange],[Purple]]
  | otherwise = concatMap (\y -> [x:y | x <- colors]) (allCodes (n-1))

-- Exercise 7 -----------------------------------------

notYetCorrect :: Move -> Code -> Bool
notYetCorrect move secret = guess /= secret
  where
    Move guess _ _ = move

solve :: Code -> [Move]
solve secret = takeWhile (\move -> notYetCorrect move secret) allcodes ++ winningmove
  where
    allcodes = map (\x ->  getMove secret x) (allCodes (length secret))
    winningmove = [ Move secret 0 0 ]

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
