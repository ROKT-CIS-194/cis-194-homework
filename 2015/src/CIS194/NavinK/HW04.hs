{-# OPTIONS_GHC -Wall #-}

module CIS194.NavinK.HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P z) (P y)
      | length z == length y = z == y
      | length z > length y = (z_trimmed == y && all (== 0) z_extra)
      | otherwise = (==) (P y) (P z)
      where
          (z_trimmed, z_extra) = splitAt (length y) z

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P y)
    | degree_y == 0 = show $ last y
    | low_order_terms == [0] = high_order_term
    | otherwise = high_order_term ++ plus' ++ show (P low_order_terms)
    where
      degree_y = length y - 1
      low_order_terms = init y
      plus'
        | length(init y) == 1 && (init y /= [0]) = " + "
        | head(reverse (init y)) == 0 = ""
        | otherwise = " + "
      high_order_term
        | last y == 0 = ""
        | degree_y == 1 = high_order_coefficient ++ "x"
        | otherwise = high_order_coefficient ++ "x^" ++ (show degree_y)
        where
          high_order_coefficient
            | last y == 1 = ""
            | last y == -1 = "-"
            | otherwise = show $ last y


-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P y) (P z) = P yz
  where
    yz = zipWith (+) padded_y padded_z
    padded_y = if length y < length z
               then y ++ (replicate (length z - length y) 0)
               else y
    padded_z = if length z < length y
               then z ++ (replicate (length y - length z) 0)
               else z


-- Exercise 5 -----------------------------------------

multiplyAndPad :: Num a => a -> Int -> [a] -> Poly a
multiplyAndPad n m l = P ((replicate m 0) ++ map (* n) l)

times :: Num a => Poly a -> Poly a -> Poly a
times (P y) (P z) = foldr plus (P [0]) (map f y_with_index)
  where
    f(a, n) = multiplyAndPad a n z
    y_with_index = zipWith (\n n' -> (n,n')) y [0..]

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P y) = P (map (* (-1)) y)
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P y) m = foldr (+) 0 (map f y_with_index)
  where
    f(i,j) = i * m^j
    y_with_index = zipWith (\n n' -> (n,n')) y [0..]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n = foldr (.) id (replicate n deriv)

-- Exercise 9 -----------------------------------------

monomial_deriv :: Num a => (a, Int) -> Poly a
monomial_deriv (z, n)
  | n == 0 = fromInteger 0
  | n == 1 = P [z]
  | otherwise = P ((replicate (n-1) 0) ++ [(fromIntegral n) * z])

instance Num a => Differentiable (Poly a) where
  deriv (P y) = foldr plus (P [0]) (map monomial_deriv y_with_index)
    where
      y_with_index = zipWith (\n n' -> (n,n')) y [0..]
