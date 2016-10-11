{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wall            #-}

module CIS194.BenS.HW07 where

import           CIS194.BenS.HW07.Cards
import           Prelude                hiding (mapM)

import           Control.Monad          hiding (liftM, mapM)
import           Control.Monad.Random
import           Data.Functor
import           Data.Monoid
import           Data.Vector            (Vector, cons, (!), (!?), (//))
import           System.Random

import qualified Data.Vector            as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= \x -> return (f x)

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = liftM2 (\x y -> v // [(i, y), (j, x)]) (v !? i) (v !? j)

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = f x >>= \y -> mapM f xs >>= \ys -> return (y:ys)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = mapM (v !?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = do
  i <- getRandomR (0, V.length v - 1)
  return (v ! i <$ guard (not (V.null v)))

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = liftM V.fromList $ replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n = liftM V.fromList . replicateM n . getRandomR

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v' = foldM go v' ixs
  where
    ixs = [V.length v' - 1, V.length v' - 2 .. 0]
    go v i = do
      j <- getRandomR (0, i)
      return (v // [(i, v ! j), (j, v ! i)])

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (V.fromList xs, x, V.fromList ys)
  where
    x        = V.unsafeHead bs
    (as, bs) = V.splitAt i v
    (xs, ys) = go (V.toList as) . go (V.toList (V.unsafeTail bs)) $ ([], [])
    go = flip . foldr $ \u (vs, ws) -> if u < x then (u:vs, ws) else (vs, u:ws)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | V.null v = v
  | otherwise =
      qsort xs V.++ V.cons x (qsort ys) where (xs, x, ys) = partitionAt v 0

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR = fmap (V.fromList . ($ [])) . go id
  where
    go f v
      | V.null v  = return f
      | otherwise = do
          (xs, x, ys) <- partitionAt v <$> getRandomR (0, V.length v - 1)
          (\g h -> g . (x:) . h) <$> go id xs <*> go id ys

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
  | V.null v = return Nothing
  | otherwise = do
      (xs, x, ys) <- partitionAt v <$> getRandomR (0, V.length v - 1)
      let j = V.length xs
      case compare i j of
        LT -> select i xs
        EQ -> return (Just x)
        GT -> select (i - j - 1) ys

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = flip Card <$> suits <*> labels

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck =
  (V.unsafeHead deck, V.unsafeTail deck) <$ guard (not (V.null deck))

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = (fmap (\(f,d) -> (f [], d)) .) . go id
  where
    go f 0 deck = return (f, deck)
    go f n deck = do
      (c, deck') <- nextCard deck
      go (f . (c:)) (n-1) deck'

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
