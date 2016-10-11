{-# OPTIONS_GHC -Wall #-}
module CIS194.ClaudioN.Lesson02 where

-- let expressions
strLength :: String -> Int
strLength []     = 0
strLength (_:xs) = let len_rest = strLength xs in
                   len_rest + 1

-- where clauses
frob :: String -> Char
frob []  = 'a'   -- len is NOT in scope here
frob str
  | len > 5   = 'x'
  | len < 3   = 'y'
  | otherwise = 'z'
  where
    len = strLength str

-- accumulators
sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums   -- the acc. starts at 0
  where go :: Int -> [Int] -> Int
        go acc [] = acc   -- empty list: return the accumulated sum
        go acc (x:xs)
         | acc >= 20 = acc
         | otherwise = go (acc + x) xs

-- sumTo20 [4,9,10,2,8]


-- Parametric polymorphism
notEmpty :: [a] -> Bool
notEmpty (_:_) = True
notEmpty []    = False


-- Total and partial functions
doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2


-- Recursion patterns
addOneToAll :: [Int] -> [Int]
addOneToAll []     = []
addOneToAll (x:xs) = x + 1 : addOneToAll xs

absAll :: [Int] -> [Int]
absAll []     = []
absAll (x:xs) = abs x : absAll xs

squareAll :: [Int] -> [Int]
squareAll []     = []
squareAll (x:xs) = x^2 : squareAll xs


map2 :: (a -> b) -> [a] -> [b]
map2 _ []     = []
map2 f (x:xs) = f x : map f xs


-- Filter
keepOnlyPositive :: [Int] -> [Int]
keepOnlyPositive [] = []
keepOnlyPositive (x:xs)
  | x > 0     = x : keepOnlyPositive xs
  | otherwise = keepOnlyPositive xs

keepOnlyEven :: [Int] -> [Int]
keepOnlyEven [] = []
keepOnlyEven (x:xs)
  | even x    = x : keepOnlyEven xs
  | otherwise = keepOnlyEven xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs


-- Fold
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

fold :: (a -> b -> b) -> b  -> [a] -> b
fold _ z []     = z
fold f z (x:xs) = f x (fold f z xs)


sum'' :: [Int] -> Int
product'' :: [Int] -> Int
length'' :: [a] -> Int

sum''     = fold (+) 0
product'' = fold (*) 1
length''  = fold addOne 0
 where addOne _ s = 1 + s

--foldr f z [a,b,c] == a `f` (b `f` (c `f` z))
--foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c


-- Functional Programming

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) f g x = f (g x)

add1Mul4 :: [Int] -> [Int]
add1Mul4 x = map ((*4) . (+1)) x

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
negateNumEvens1 :: [Int] -> Int
negateNumEvens1 x = negate (length (filter even x))

negateNumEvens2 :: [Int] -> Int
negateNumEvens2 x = negate $ length $ filter even x

-- anonymous functions
duplicate1 :: [String] -> [String]
duplicate1 = map dup
  where dup x = x ++ x

duplicate2 :: [String] -> [String]
duplicate2 = map (\x -> x ++ x)


-- Currying and Partial Application
g :: Int -> Int -> Int
g x y = 2*x + y

g' :: Int -> (Int -> Int)
g' x y = 2*x + y

g'' :: (Int,Int) -> Int
g'' (x,y) = 2*x + y

-- curry
schönfinkel :: ((a,b) -> c) -> a -> b -> c
schönfinkel f x y = f (x,y)

-- uncurry
unschönfinkel :: (a -> b -> c) -> (a,b) -> c
unschönfinkel f (x,y) = f x y


-- wholemeal programming
foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map ((+2) . (*7)) . filter (>3)
