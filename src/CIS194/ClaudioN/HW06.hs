{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}
{-# LANGUAGE BangPatterns #-}

module CIS194.ClaudioN.HW06 where

import Data.List
--import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x s) = Cons (f x) (fmap f s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x $ sRepeat x

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x $ sIterate f (f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x s) s2 = Cons x $ sInterleave s2 s

sTake :: Int -> Stream a -> [a]
sTake n _ | n <= 0 = []
sTake n (Cons x s) = x : sTake (n - 1) s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate succ 0

-- fascinating:
-- https://oeis.org/A007814
-- https://oeis.org/A001511 (switch [0..] for [1..])
ruler :: Stream Integer
ruler = foldr1 sInterleave $ map sRepeat [0..]

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate (\r -> (1103515245 * r + 12345) `mod` 2147483648)

-- Exercise 8 -----------------------------------------

-- ghc HW06.hs -rtsopts -main-is CIS194.ClaudioN.HW06
-- ./HW06 +RTS -s
-- ./HW06 +RTS -h -i0.001
-- hp2ps -c HW06.hp

{- Total Memory in use: 237 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax [x] = Just (x, x)

{- Total Memory in use: 333 MB -}
--minMax (x:xs) = minMax xs >>= (\(lo, hi) -> Just (min lo x, max hi x))

{- Total Memory in use: 1 MB -}
minMax (x:xs) = Just $ foldl' (\(!lo, !hi) v  -> (min lo v, max hi v)) (x, x) xs

{- Total Memory in use: 1 MB -}
-- minMax (x:xs) = Just $ go x x xs
--   where go lo hi []       = (lo, hi)
--         go !lo !hi (v:vs) = go (min lo v) (max hi v) vs


main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------
data Matrix = M2x2 Integer Integer Integer Integer
  deriving (Eq, Show)

instance Num (Matrix) where
    (*) = times

times :: Matrix -> Matrix -> Matrix
times (M2x2 a1 b1 c1 d1) (M2x2 a2 b2 c2 d2)
  = M2x2 (a1 * a2 + b1 * c2) (a1 * b2 + b1 * d2)
         (c1 * a2 + d1 * c2) (c1 * b2 + d1 * d2)

fastFib :: Int -> Integer
fastFib 0 = 0
fastFib n = x
  where M2x2 _ x _ _ = mF^n
        mF = M2x2 1 1
                  1 0

--main = print $ take 100 $ show $ fastFib 1000000
