{-# OPTIONS_GHC -Wall #-}
module CIS194.ClaudioN.Lesson03 where

import Data.Char ( toUpper )

-- Hackage
--
-- > cabal update
-- > cabal install text-1.1.1.3


-- Enumeration types
--
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show


shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]


-- Pattern matching
--
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _    = True


-- Beyond numerations
--
data FailableDouble = Failure
                    | OK Double
  deriving Show

ex01 = Failure
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d


-- Store a person's name, age, and favorite Thing.
data Person = Person String Int Thing
  deriving Show

richard :: Person
richard = Person "Richard" 32 Ship

stan :: Person
stan  = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a


-- Algebraic data types in general
--
data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- baz richard

checkFav :: Person -> String
checkFav (Person n _ Ship) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)    = n ++ ", your favorite thing is lame."

-- checkFav richard
-- checkFav stan


-- Case expressions
--
ex03 = case "Hello" of
           []      -> 3
           ('H':s) -> length s
           _       -> 7

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d


-- Polymorphic data types
--
data LogMessage = LogMessage Int String
data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM
data MaybeInt = ValidInt Int
              | InvalidInt

-- generalising to:
data Maybe a = Just a
             | Nothing

example_a :: Maybe Int -> Int
example_a (Just n) = n
example_a Nothing  = (-1)

example_b :: LogMessage -> Maybe String
example_b (LogMessage severity s) | severity >= 50 = Just s
example_b _                                        = Nothing


-- Recursive data types
--
data List t = Empty | Cons t (List t)

lst1 :: List Int
lst1 = Cons 3 (Cons 5 (Cons 2 Empty))

lst2 :: List Char
lst2 = Cons 'x' (Cons 'y' (Cons 'z' Empty))

lst3 :: List Bool
lst3 = Cons True (Cons False Empty)

intListProd :: List Int -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l


data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
