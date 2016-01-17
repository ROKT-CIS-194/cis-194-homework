{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wall            #-}
module CIS194.ClaudioN.HW07 where

import           Prelude                    hiding (mapM)
import           CIS194.ClaudioN.HW07.Cards

import           Control.Monad              hiding (mapM, liftM)
import           Control.Monad.Random
import           Data.Functor
import           Data.Maybe                 (fromJust)
import           Data.Monoid
import           Data.Vector                (Vector, cons, (!), (!?), (//))
import           System.Random

import qualified Data.Vector                as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= \a -> return (f a)

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = liftM2 (\vi vj -> v//[(i, vj), (j, vi)]) (v !? i) (v !? j)

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM f xs = foldr (liftM2 (:)) (return []) (map f xs)
mapM = (sequence .) . map

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = mapM (v !?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (v !?) <$> getRandomR (0, pred $ length v)

-- evalRandIO $ randomElt (V.fromList [])
-- evalRandIO $ randomElt (V.fromList [0])
-- evalRandIO $ randomElt (V.fromList [0,1])
-- evalRandIO $ randomElt (V.fromList [0..9])

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
--randomVec n = liftM V.fromList $ replicateM n getRandom
randomVec n = V.fromList <$> replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n range = V.fromList <$> replicateM n (getRandomR range)

-- evalRandIO $ randomVec 3
-- evalRandIO $ randomVecR 3 (0,10)

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- liftM :: Monad m => (a -> b) -> m a -> m b
--
-- So fmap ~= liftM?
-- Is this duplication because monads are not functors in Haskell?

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = foldl (\v2 (i, j) -> fromJust $ swapV i j v2) v <$> mapM pair is
  where n  = length v
        is = [n-1,n-2..1]
        pair i = ((,) i) <$> getRandomR (0, i)

-- evalRandIO $ shuffle (V.fromList [0..9])
-- evalRandIO $ length <$> filter (== 0) <$> map V.head <$> replicateM 100000 (shuffle (V.fromList [0..2]))

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (lt, p, gte)
  where p   = v ! i
        lt  = V.ifilter (\vi va -> va <  p && vi /= i) v
        gte = V.ifilter (\vi va -> va >= p && vi /= i) v

-- Cue Ben: "This makes two passes through the vector."
-- FIXME: Could use V.partition, with some shuffling of the ith value?


-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y < x]
                   <> (x : quicksort [y | y <- xs, y >= x])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | V.null v  = V.empty
  | otherwise = qsort [y | y <- xs, y < x]
                <> (V.cons x $ qsort [y | y <- xs, y >= x])
  where x  = V.head v
        xs = V.tail v

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | V.null v = return V.empty
  | otherwise = liftM (partitionAt v) ri >>= go
  where n  = length v
        ri = getRandomR(0, n - 1)
        go (lt, p, gte) = liftM2 (\v1 v2 -> v1 <> p `cons` v2) (qsortR lt) (qsortR gte)

-- let v = V.fromList $ reverse [1..10000]
-- qsort v
-- evalRandIO $ qsortR v

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
  | V.null v = return Nothing
  | otherwise = liftM (partitionAt v) ri >>= go
  where n  = length v
        ri = getRandomR(0, n - 1)
        go (lt, p, gte)
          | i < nlt = select i lt
          | i > nlt = select (i - nlt - 1) gte
          | otherwise = return $ Just p
          where nlt = V.length lt

-- Exercise 8b/9b -----------------------------------------
-- DISCUSS: more idiomatic way to express this commonality?
partover :: Ord a => ((Vector a, a, Vector a) -> Rnd b) -> b -> Vector a -> Rnd b
partover f b v
  | V.null v = return b
  | otherwise = liftM (partitionAt v) ri >>= f
  where n  = length v
        ri = getRandomR(0, n - 1)

hqsortR :: Ord a => Vector a -> Rnd (Vector a)
hqsortR = partover go V.empty
  where go (lt, p, gte) = liftM2 (\v1 v2 -> v1 <> p `cons` v2) (hqsortR lt) (hqsortR gte)

hselect :: Ord a => Int -> Vector a -> Rnd (Maybe a)
hselect i = partover go Nothing
  where go (lt, p, gte)
          | i < nlt = hselect i lt
          | i > nlt = hselect (i - nlt - 1) gte
          | otherwise = return $ Just p
          where nlt = V.length lt

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card l s | s <- suits, l <- labels]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
  | V.null d  = Nothing
  | otherwise = Just (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = go []
  where go cs 0 d = Just (cs, d)
        go cs n d = do
          (c, d') <- nextCard d
          go (cs ++ [c]) (n - 1) d'

-- getCards  1 allCards
-- getCards 52 allCards
-- getCards 53 allCards

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
