{-# OPTIONS_GHC -Wall #-}

module CIS194.BenS.HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P xs == P ys = go xs == go ys
      where go = foldr (\a as -> if null as && 0 == a then as else a:as) []

-- Exercise 3 -----------------------------------------

polyToStrings :: (Num a, Eq a, Show a) => Poly a -> [String]
polyToStrings (P ns) = foldl f [] (zip [(0::Integer)..] ns)
  where
    f ss (_,  0) = ss
    f ss (0,  n) = show n:ss
    f ss (1,  n) = showN n:ss
    f ss (e,  n) = (showN n ++ "^" ++ show e):ss
    showN   1  =  "x"
    showN (-1) = "-x"
    showN   n  = show n ++ "x"

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show = foldr f [] . polyToStrings
      where f a as = if null as then a else a ++ " + " ++ as

-- Exercise 4 -----------------------------------------

zipKeep :: (a -> a -> a) -> [a] -> [a] -> [a]
zipKeep _ xs [] = xs
zipKeep _ [] ys = ys
zipKeep f (x:xs) (y:ys) = f x y : zipKeep f xs ys

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P (zipKeep (+) xs ys)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = foldr plus (P []) mults
  where mults = zipWith (\x i -> P (replicate i 0 ++ map (x *) ys)) xs [0..]

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P xs) = P (map negate xs)
    fromInteger = P . (:[]) . fromInteger
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P cs) x = sum (zipWith (*) cs (iterate (*x) 1))

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n x = iterate deriv x !! n

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P xs) = P (zipWith (\i x -> fromInteger i * x) [1..] (drop 1 xs))
