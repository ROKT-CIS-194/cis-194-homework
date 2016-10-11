{-# OPTIONS_GHC -Wall #-}
module CIS194.NavinK.HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fib(n-1) + fib(n-2)


fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1:1: zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList s = x:streamToList(y) where
  (Cons x y) = s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
  fmap f (Cons x s) = Cons (f(x)) (fmap f s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = (Cons x (sRepeat x))

sIterate :: (a -> a) -> a -> Stream a
sIterate f o = (Cons o (sIterate f (f o)))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x y) s = Cons x (sInterleave s y)

sTake :: Int -> Stream a -> [a]
sTake n (Cons x y)
  | n == 1 = [x]
  | otherwise = x : sTake (n-1) y

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = foldr1 sInterleave (streamToList(streamNats))
  where streamNats = fmap sRepeat nats

-- Exercise 7 -----------------------------------------

-- Rn = (1103515245 × Rn−1 + 12345) mod 2147483648
-- | Implementation of C rand
rand :: Int -> Stream Int
rand r_0 = sIterate (\r -> (1103515245 * r + 12345) `mod` 2147483648) r_0

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 119 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax s
  | s == [] = Nothing
  | otherwise = go (head (s)) (head (s)) (tail (s))
  where go min max [] = Just(min, max)
        go min max (x:xs) = min `seq` max `seq` go (minimum [x,min]) (maximum [x, max]) xs

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = M Integer Integer Integer Integer deriving (Show, Eq)

instance Num Matrix where
  (M a_11 a_12 a_21 a_22) * (M b_11 b_12 b_21 b_22) = M x_11 x_12 x_21 x_22
    where x_11 = a_11 * b_11 + a_12 * b_21
          x_12 = a_11 * b_12 + a_12 * b_22
          x_21 = a_21 * b_11 + a_22 * b_21
          x_22 = a_21 * b_21 + a_22 * b_22

a_11 :: Matrix -> Integer
a_11 (M x_11 _ _ _) = x_11

fastFib :: Int -> Integer
fastFib n = a_11 (M 1 1 1 0 ^ n)
