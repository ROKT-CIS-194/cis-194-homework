{-# OPTIONS_GHC -Wall #-}

module CIS194.ClaudioN.HW04 where

import Data.List

-- FIXME: adopt smart constructor, to canonicalize representaton (i.e. remove leading zeros)
newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P p1) == (P p2) = dropTerms p1 == dropTerms p2
      where dropTerms = dropWhile (== 0) . reverse

-- Exercise 3 -----------------------------------------

-- FIXME: pattern matching on terms would resolve need for special handling of 0 cases
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P[]) = "0" -- FIXME: when all 0?
    show (P[0]) = "0"
    show (P cs) = intercalate " + " $ reverse $ terms where
      terms = filter (not . null) $ map showTerm $ zip cs [(0::Integer)..]
      showTerm (c, e)
        | c == 0 = ""
        | e == 0 = show c
        | c ==  1 = showExp e
        | c == -1 = "-" ++ showExp e
        | otherwise = show c ++ showExp e
      showExp e
        | e == 1 = "x"
        | otherwise = "x^" ++ show e

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2) = P $ add p1 p2
  where add (a:as) (b:bs) = (a+b) : add as bs
        add as [] = as
        add [] bs = bs

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P p1) (P p2) = sum $ mul p1 p2
  where mul [] _ = []
        mul (a:as) bs = P (map (* a) bs) : mul as (0:bs)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate        = (* P [-1])
    fromInteger a = P [fromInteger a]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

-- FIXME: iterate approch is nicer
applyP :: Num a => Poly a -> a -> a
applyP (P cs) a = sum $ map (\(c,e) -> c * a^e) $ zip cs [(0::Integer)..]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n = foldr (.) id (replicate n deriv)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:cs)) = P (map (\(c, e) -> c * fromInteger e) $ zip cs [1..])
