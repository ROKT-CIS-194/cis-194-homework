{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wall            #-}

module CIS194.NavinK.HW07 where

import           Prelude                    hiding (mapM)
import           CIS194.NavinK.HW07.Cards

import           Control.Monad              hiding (mapM, liftM)
import           Control.Monad.Random
import           Data.Functor
import           Data.Monoid
import           Data.Vector                (Vector, cons, (!), (!?), (//))
import           System.Random

import qualified Data.Vector                as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = mx >>= \x -> return $ f x
-- liftM f mx = do
--   x <- mx
--   return $ f x

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV n_1 n_2 v = liftM2 (\x y -> v // [(n_1, y), (n_2, x)]) (v !? n_1) (v !? n_2) -- from BenS
-- my original:
-- swapV n_1 n_2 v = do
--   v_n_1 <- liftM2(!?) (Just v) (Just n_1)
--   v_n_2 <- liftM2(!?) (Just v) (Just n_2)
--   v_1 <- v_n_1
--   v_2 <- v_n_2
--   liftM2(//) (Just v) (Just [(n_1, v_2), (n_2, v_1)])

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f l = sequence $ map f l

getElts :: [Int] -> Vector a -> Maybe [a]
getElts l v = mapM (\n -> ((!?) v n)) l

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = do
  random_index <- getRandomR (0, (length v) - 1)
  return $ (!?) v random_index

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = liftM V.fromList $ (replicateM n getRandom)

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n (lo, hi)  = liftM V.fromList $ (replicateM n (getRandomR (lo, hi)))

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = foldM f v [1..length(v)]
  where f :: Vector a -> Int -> Rnd (Vector a)
        f v k = do
          let i = length(v) - k
          j <- getRandomR (0, i)
          return (v // [(j, ((!) v i)), (i, ((!) v j))])

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (less, ((!) v i), more)
  where (more, less) = V.partition (>= ((!) v i)) all_but_ith
        (first_i, rest) = V.splitAt i v
        all_but_ith = (V.++) first_i (V.tail rest)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v = case (V.null v) of
  True -> v
  False -> qsort [ y | y <- xs, y < x ] <> (V.cons x (qsort [y | y <- xs, y >= x ]))
    where
      x = V.head v
      xs = V.tail v

  -- False -> qsort l <> V.cons x (qsort m)
  --   where
  --     (l, x, m) = partitionAt v 0

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v = case (V.null v) of
  True -> return v
  False -> do
    random_index <- getRandomR(0, (length v) - 1)
    let (l, x, m) = partitionAt v random_index
    l_R <- qsortR l
    m_R <- qsortR m
    return $ (l_R <> V.cons x m_R)

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
  | i < 0 || i > ((length v) - 1)= return Nothing
  | otherwise = do
    random_index <- getRandomR(0, (length v) - 1)
    let handle(l, x, m)
          | (length l - i) == 0 = return $ Just x
          | (length l) > i = select i l
          | otherwise = select (i - (length l) - 1) m
    handle(partitionAt v random_index)


-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card label suit | suit <- suits, label <- labels]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d =
  case (V.null d) of
    True -> Nothing
    False -> Just (head d_l, V.fromList(tail d_l)) where d_l = V.toList d

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n d
  | n <= 0 || n > (length d) = Nothing
  | n == 1 = do
      (card, remaining) <- nextCard d
      Just ([card], remaining)
  | otherwise = do
      (first_card, remaining_deck) <- nextCard d
      (remaining_cards, remainder) <- getCards (n-1) remaining_deck
      Just ([first_card] ++ remaining_cards, remainder)

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
