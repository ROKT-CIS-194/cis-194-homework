{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week1 where

import CodeWorld
import Control.Applicative
import Control.Category
import Data.Bifunctor (bimap)
import Data.List (find)

import Prelude hiding (id, (.))

-- #1 TRAFFIC LIGHTS

-- State Machine
data Machine i o = Done | Step (i -> (o, Machine i o))

instance Category Machine where
  id = Step $ \i -> (i, id)
  Step gm . Step fm = Step $ \i ->
    let (fo, fk) = fm i; (go, gk) = gm fo in (go, gk . fk)
  _ . _ = Done

instance Functor (Machine i) where
  fmap _ Done = Done
  fmap f (Step m) = Step $ bimap f (fmap f) . m

instance Applicative (Machine i) where
  pure x = Step $ \_ -> (x, pure x)
  Step fm <*> Step xm = Step $ \i ->
    let (fo, fk) = fm i; (xo, xk) = xm i in (fo xo, fk <*> xk)
  _ <*> _ = Done

instance Alternative (Machine i) where
  empty = Done
  Done <|> y = y
  Step m <|> y = Step $ fmap (<|> y) . m

nSteps :: Int -> Machine i o -> Machine i o
nSteps _ Done = Done
nSteps n (Step m) = if n <= 0 then Done else Step $ fmap (nSteps (n-1)) . m

data Light = Red | Yellow | Green deriving (Eq, Show)

lightCycle :: Machine i [Light]
lightCycle =
  loopMachines [(30, [Green]), (10, [Yellow]), (30, [Red]), (10, [Yellow, Red])]
  where
    loopMachines = foldr1 (<|>) . cycle . map (uncurry f)
    f n = nSteps n . pure

topCircle, midCircle, botCircle :: Color -> Picture
topCircle c = coloured c (translated 0 3.5 (solidCircle 1))
midCircle c = coloured c (translated 0 0 (solidCircle 1))
botCircle c = coloured c (translated 0 (-3.5) (solidCircle 1))

trafficLight :: [Light] -> Picture
trafficLight ls =
  topCircle (maybe black (const red) (find (== Red) ls))
  & midCircle (maybe black (const yellow) (find (== Yellow) ls))
  & botCircle (maybe black (const green) (find (== Green) ls))
  & rectangle 2.5 9.5

exercise1 :: IO ()
exercise1 = simulationOf (blank, machine) (\dt (_, Step m) -> m dt) fst
  where
    machine = trafficLight <$> lightCycle


-- #2 BLOSSOMS

tree :: Picture -> Int -> Picture
tree blossom 0 = blossom
tree blossom n =
  let k = tree blossom (n-1) in
  translated 0 1 (rotated (pi/10) k & rotated (-pi/10) k) & path [(0,0),(0,1)]

exercise2 :: IO ()
exercise2 = animationOf (\t -> tree (blossom t) 6)
  where
    blossom t = coloured (RGBA (sin t) (cos t) 1 1) $ solidCircle (0.15 * sin t)


-- #3 SOKOBAN

circ, square :: Color -> Picture
circ col = coloured col (solidCircle 0.35)
square col = coloured col (solidRectangle 1.02 1.02)

wall, ground, storage, box :: Picture
wall = square (grey 0.4)
ground = square yellow
storage = circ black & square yellow
box = circ (mixed black brown) & square brown

drawTile :: Int -> Picture
drawTile = (ps !!)
  where
    ps = [blank, wall, ground, storage, box] ++ repeat blank

maze :: Int -> Int -> Int
maze x y
  | abs x >  4 || abs y >  4 = 0
  | abs x == 4 || abs y == 4 = 1
  | x == 2     && y <= 0     = 1
  | x == 3     && y <= 0     = 3
  | x >= -2    && y == 0     = 4
  | otherwise                = 2

pos :: Int -> Int -> (Double, Double)
pos x y = (fromIntegral x, fromIntegral y)

pictureOfMaze :: Picture
pictureOfMaze =
  mconcat [f x y | x <- [-10..10], y <- [-10..10]]
  where
    f x y =
      let (tx,ty) = pos x y
      in translated tx ty (drawTile (maze x y))

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
