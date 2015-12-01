{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
module CIS194.BenS.HW06 where

import           Control.Applicative (liftA3)
import           Control.Monad       (guard)
import           Data.Functor
import           Data.List
import           Data.Profunctor

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

data FoldF b a
  = forall x. Fold (b -> x -> x) !x (x -> a)

instance Functor (FoldF b) where
  fmap f (Fold g s k) = Fold g s (f . k)

instance Profunctor FoldF where
  dimap g f (Fold ff s k) = Fold (ff . g) s (f . k)

data Fold b a
  = Pure a
  | forall x. Ap (FoldF b (x -> a)) !(Fold b x)

instance Functor (Fold b) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Ap fm xm) = Ap (fmap (f .) fm) xm

instance Profunctor Fold where
  dimap _ f (Pure x) = Pure (f x)
  dimap g f (Ap fm xm) = Ap (dimap g (f .) fm) (lmap g xm)

instance Applicative (Fold b) where
  pure = Pure
  Pure f <*> xm = fmap f xm
  Ap fm xm <*> ym = Ap (fmap uncurry fm) ((,) <$> xm <*> ym)

runFold :: Fold b a -> [b] -> a
runFold = (finish .) . foldl' step
  where
    step :: Fold b a -> b -> Fold b a
    step (Pure x) _ = Pure x
    step (Ap (Fold f s k) xm) x = Ap (Fold f (f x s) k) (step xm x)

    finish :: Fold b a -> a
    finish (Pure x) = x
    finish (Ap (Fold _ s k) xm) = k s (finish xm)

fold :: (b -> a -> a) -> a -> Fold b a
fold f s = Ap (Fold f s const) (Pure ())

foldMin :: Ord a => Fold a (Maybe a)
foldMin = fold (\x -> maybe (Just x) (\y -> y `seq` Just (min x y))) Nothing

foldMax :: Ord a => Fold a (Maybe a)
foldMax = fold (\x -> maybe (Just x) (\y -> y `seq` Just (max x y))) Nothing

foldCount :: Num a => Fold b a
foldCount = fold (const (+1)) 0

foldSum :: Num a => Fold a a
foldSum = fold (+) 0

foldAvg :: (Ord a, Fractional a) => Fold a (Maybe a)
foldAvg = f <$> foldSum <*> foldCount
  where
    f total count = (total / count) <$ guard (count > 0)

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int, Double)
minMax =
  runFold (liftA3 (,,) <$> foldMin <*> foldMax <*> lmap fromIntegral foldAvg)

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
