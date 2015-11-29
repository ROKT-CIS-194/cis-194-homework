{-# OPTIONS_GHC -Wall #-}
module CIS194.BenS.HW06 where

import           Data.Functor
import           Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n = if n < 2 then 1 else fib (n-2) + fib (n-1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : zipWith (+) fibs2 (0:fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid a => Monoid (Stream a) where
    mempty = Cons mempty mempty
    mappend (Cons x xs) (Cons y ys) = Cons (mappend x y) (mappend xs ys)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) ys = Cons x (sInterleave ys xs)

sTake :: Int -> Stream a -> [a]
sTake n (Cons x xs) = if n < 1 then [] else x : sTake (n-1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = foldr1 sInterleave (map sRepeat [0..])

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = seed' `seq` Cons seed' (rand seed')
  where
    seed' = (1103515245 * seed + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 202 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = go Nothing
  where
    go acc [] = acc
    go acc (x:xs) = case acc of
      Nothing      -> go (Just (x, x)) xs
      Just (lo,hi) -> lo `seq` (hi `seq` go (Just (min lo x, max hi x)) xs)

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = Matrix Integer Integer Integer Integer
  deriving (Eq, Show)

instance Num Matrix where
  fromInteger n = Matrix n 0 0 0
  Matrix x0 x1 x2 x3 * Matrix y0 y1 y2 y3 =
    Matrix (x0*y0 + x1*y2) (x0*y1 + x1*y3)
           (x2*y0 + x3*y2) (x2*y1 + x3*y3)

f0 :: Matrix
f0 = Matrix 1 1 1 0

fastFib :: Int -> Integer
fastFib n
  | even n =
    case (f0 ^ (n `div` 2)) ^ (2::Int) of Matrix x _ _ _ -> x
  | otherwise =
    case f0 * (f0 ^ ((n-1) `div` 2)) ^ (2::Int) of Matrix x _ _ _ -> x
