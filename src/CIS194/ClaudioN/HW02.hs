{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module CIS194.ClaudioN.HW02 where
--module Main where

import Data.List
import Data.Map (elems, fromListWith, Map)
import Data.Function.Memoize
import Control.Parallel.Strategies

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

deriveMemoizable ''Peg

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq, Ord)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

countIf :: (a -> Bool) -> [a] -> Int
countIf p xs = length $ filter p xs

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches s g = countIf id $ zipWith (==) s g

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times it occurs in ys
countColors :: Code -> [Int]
countColors code = map (\c -> countIf (c ==) code) colors

countColors' :: Code -> [Int]
countColors' = memoize countColors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches s g = sum $ zipWith min (countColors' s) (countColors' g)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
-- Note: first code is secret, second code is guess
getMove :: Code -> Code -> Move
getMove s g = Move g e ne
  where e = exactMatches s g
        ne = (matches s g) - e

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move g _ _) c = (getMove c g) == m

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
--filterCodes m cs = filter (isConsistent m) cs
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

addOneMore :: [Code] -> [Code]
addOneMore codes = concatMap (\c -> map (c :) codes) colors

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = addOneMore $ allCodes (n - 1)

-- Exercise 7 -----------------------------------------

-- Given the secret, and a list of remaining possibilities, produces
-- list of moves
solver :: Code -> [Code] -> [Move]
solver _ [] = []
-- solver s (g:gs)
--   | (s == g) = [move]
--   | otherwise = move : (solver s $ filterCodes move gs)
--   where move = getMove s g
solver s (g:gs) = move : nextMoves
  where move = getMove s g
        nextMoves
          | (s == g) = []
          | otherwise = solver s $ filterCodes move gs

solve :: Code -> [Move]
solve s = solver s (allCodes $ length s)

-- Bonus ----------------------------------------------
maximumsBy :: (Ord b) => (a -> b) -> [a] -> [a]
maximumsBy f xs = let vals = map f xs
                      maxVal = maximum vals
                      fxs = zip xs vals
                  in map fst $ filter ((== maxVal) . snd) fxs

freq :: (Ord a) => [a] -> Map a Int
freq xs = fromListWith (+) [(x, 1) | x <- xs]

-- c.f. https://en.wikipedia.org/wiki/Mastermind_(board_game)#Five-guess_algorithm
-- 1) Create the set S of 1296 possible codes, 1111,1112,.., 6666.
allCodes4 :: [Code]
allCodes4 = allCodes 4

-- 2) Start with initial guess 1122 (Knuth gives examples showing that
-- some other first guesses such as 1123, 1234 do not win in five tries
-- on every code).
firstGuess :: Code
firstGuess = [Red, Red, Green, Green]

-- 3) Play the guess to get a response of colored and white pegs.
-- playFirstGuess :: Code -> Move
-- playFirstGuess s = getMove s firstGuess

-- 4) If the response is four colored pegs, the game is won, the
-- algorithm terminates.
finished :: Move -> Bool
finished (Move c exact 0) = (exact == length c)
finished _ = False

-- 5) Otherwise, remove from S any code that would not give the
-- same response if it (the guess) were the code.
-- remaining :: Code -> [Code]
-- remaining s = filterCodes (playFirstGuess s) allCodes4

-- 6) Apply minimax technique to find a next guess as follows:
--  For each possible guess, that is, any unused code of the 1296 not
--  just those in S, calculate how many possibilities in S would be
--  eliminated for each possible colored/white peg score.
--
--  The score of a guess is the minimum number of possibilities it
--  might eliminate from S. A single pass through S for each unused code
--  of the 1296 will provide a hit count for each colored/white peg score
--  found; the colored/white peg score with the highest hit count will
--  eliminate the fewest possibilities
--
--  Calculate the score of a guess by using:
--    "minimum eliminated" = "count of elements in S" - "highest hit count".
scoreGuess :: [Code] -> Code -> Int
scoreGuess s g = n - (maximum hitCounts)
  where n = length s
        hitCounts = elems $ freq $ map (\c -> getMove c g) s

-- From the set of guesses with the maximum score, select one as the
-- next guess, choosing a member of S whenever possible. (Knuth follows
-- the convention of choosing the guess with the least numeric value e.g.
-- 2345 is lower than 3456. Knuth also gives an example showing that in
-- some cases no member of S will be among the highest scoring guesses
-- and thus the guess cannot win on the next turn, yet will be necessary
-- to assure a win in five.)
nextGuess :: [Code] -> [Code] -> Code
nextGuess u s = head $ concat [maxInS, maxGuesses]
  where maxGuesses = maximumsBy (scoreGuess s) u
        maxInS = intersect maxGuesses s

-- 7) Repeat from step 3.
fiveGuessSolver :: Code -> Code -> [Code] -> [Code] -> [Move]
fiveGuessSolver c g u s = move : nextMoves
  where move = getMove c g
        nextMoves
         | finished move = []
         | otherwise = let s' = filterCodes move s -- Step 5
                           g' = nextGuess u s'
                       in fiveGuessSolver c g' (delete g' u) s'

fiveGuess :: Code -> [Move]
fiveGuess c = fiveGuessSolver c firstGuess (delete firstGuess s) s
  where s = allCodes $ length c

-- take 1 $ drop 777 $ (map (fiveGuess) allCodes4)
-- map (length . fiveGuess) allCodes4

main :: IO ()
--main = print $ take 10 $ map (length . fiveGuess) allCodes4
--main = print $ map (length . fiveGuess) allCodes4
main = print $ (parMap rseq) (length . fiveGuess) allCodes4


-- ghc -O2 --make CIS194/ClaudioN/HW02.hs -rtsopts
-- time ./CIS194/ClaudioN/HW02  +RTS -sstderr
--
-- ghc -O2 --make CIS194/ClaudioN/HW02.hs -prof -auto-all -caf-all -fforce-recomp
-- time ./CIS194/ClaudioN/HW02  +RTS -p
--
-- 7m59s later (5m04s, after memoizing countColors)
-- [4,4,4,4,4,5,4,1,3,4,4,5,4,4,5,3,4,4,5,4,4,5,4,4,4,4,5,4,4,5,4,5,5,5,5,5,4,3,2,4,4,5,3,4,2,4,4,4,3,3,4,3,3,5,4,4,5,4,4,4,4,4,5,4,4,3,4,4,5,5,4,5,5,4,5,3,3,5,4,3,4,3,4,5,4,4,5,4,5,5,3,4,4,2,5,4,4,3,5,5,5,4,4,4,5,5,5,5,4,4,4,4,5,5,4,4,4,4,5,5,4,4,5,3,4,4,5,5,4,4,4,4,5,5,4,4,5,4,4,4,5,5,5,5,4,4,5,5,5,3,4,4,4,4,4,4,4,4,5,4,5,5,5,5,5,4,4,4,4,4,5,5,5,5,4,4,5,5,5,4,5,4,5,5,5,5,4,4,4,4,5,5,4,4,5,4,4,4,5,5,5,5,5,5,4,5,5,5,5,5,4,5,5,5,5,4,4,3,4,4,4,5,3,4,4,4,4,4,3,3,4,4,4,5,4,4,5,4,4,5,4,4,5,4,4,5,4,4,5,5,4,5,2,4,3,4,4,4,4,4,4,4,4,4,4,5,5,3,4,4,4,5,4,4,4,4,4,4,4,4,4,3,4,4,5,5,4,5,3,4,4,3,4,4,4,5,4,4,4,5,4,4,5,4,4,5,4,5,4,2,5,4,5,3,4,5,5,5,4,4,5,5,5,5,4,3,3,4,3,4,4,5,4,5,5,5,4,5,5,3,4,4,4,5,4,4,4,4,4,5,4,4,5,4,4,4,5,5,5,5,4,4,5,5,4,4,4,4,4,4,3,5,4,5,5,4,5,5,4,5,5,4,4,4,4,4,5,4,5,5,4,4,5,4,5,4,4,5,5,5,5,5,4,4,5,5,5,5,5,5,5,4,4,4,5,5,5,5,5,5,5,5,5,4,5,5,5,5,5,5,5,4,5,4,5,4,5,5,4,3,5,4,4,4,4,5,5,4,3,5,4,3,4,4,3,5,4,4,4,4,5,4,5,5,5,5,5,5,3,4,4,4,3,5,4,4,5,5,4,4,4,5,5,4,3,5,4,5,4,4,3,5,4,5,4,4,5,4,5,5,5,5,5,5,4,4,5,5,3,4,4,5,5,4,3,4,4,4,5,4,4,4,5,5,4,4,2,4,4,4,5,4,5,4,5,5,4,5,4,4,4,4,4,4,5,4,4,4,4,4,5,4,5,5,5,5,3,5,4,4,3,5,5,4,5,5,3,3,4,4,5,5,4,5,5,4,4,4,5,5,5,3,3,4,5,5,5,3,5,5,5,4,5,4,5,5,5,4,5,5,4,4,5,5,5,4,5,5,5,4,4,5,4,5,5,4,5,5,5,5,5,4,5,5,5,5,4,5,5,3,5,5,4,5,5,5,5,5,5,5,5,5,5,5,4,5,4,4,5,4,4,4,5,5,4,4,4,4,4,5,4,4,4,3,4,4,5,5,4,4,5,5,4,4,4,4,5,5,4,4,4,5,5,5,4,4,3,4,5,5,4,5,5,5,5,5,4,5,4,3,4,4,4,5,4,4,5,5,4,5,4,4,5,5,4,5,4,5,5,5,5,4,5,4,4,5,4,4,5,4,4,5,5,5,5,5,4,5,5,5,4,5,4,5,5,5,4,4,4,4,4,4,5,4,5,4,4,5,4,4,5,4,5,5,4,4,5,4,4,4,4,5,4,5,5,5,5,4,5,4,5,5,4,5,5,5,5,5,5,4,5,5,5,5,4,3,4,5,5,4,4,3,4,5,5,5,3,4,4,4,4,4,4,4,5,4,4,4,5,5,5,4,5,5,4,5,4,4,5,4,4,5,5,5,4,4,4,5,5,5,5,5,5,5,5,4,5,5,5,5,4,4,4,4,3,4,5,4,5,5,4,5,5,5,5,4,4,5,5,5,4,5,4,4,4,5,4,5,5,5,5,5,5,5,5,4,5,5,4,5,5,5,4,5,4,4,4,5,5,5,4,5,5,4,3,4,4,4,5,4,5,5,4,5,5,5,5,5,4,5,5,4,5,5,4,4,5,5,4,5,4,4,4,5,5,5,4,4,5,4,5,5,5,5,5,4,5,5,4,4,5,4,5,5,5,5,4,5,5,5,5,5,5,5,5,5,4,4,5,5,4,5,4,4,5,5,5,5,4,5,5,5,5,5,5,5,4,4,5,4,4,4,5,5,5,5,5,5,4,5,5,5,5,5,5,5,5,5,4,5,5,5,3,5,5,4,5,5,3,5,5,5,4,5,5,5,5,5,5,5,5,5,4,4,4,5,5,4,4,4,5,5,4,4,4,5,4,4,5,5,4,4,4,4,5,5,4,4,5,5,5,4,4,4,5,5,5,5,4,4,4,5,5,4,4,4,5,5,4,4,5,4,5,5,5,5,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,5,5,4,5,5,5,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,5,5,5,5,4,5,5,5,4,5,5,4,5,5,5,4,5,5,5,5,5,4,5,5,5,5,5,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,5,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,4,5,5,5,5,5,5,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,5,4,4,5,4,5,4,4,4,5,4,5,4,5,5,5,3,5,4,5,5,5,5,5,5,4,4,5,5,4,5,4,4,5,5,5,3]
-- max  => 5
-- sum  => 5801  (woohoo, as per: http://arxiv.org/pdf/1305.1010.pdf)
-- mean => 4.476


-- ghc -O2 --make CIS194/ClaudioN/HW02.hs -rtsopts
-- time ./CIS194/ClaudioN/HW02 +RTS -N8
-- woot! => 1m15s
