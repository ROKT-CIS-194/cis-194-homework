{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week1 where

import CodeWorld

-- TRAFFIC LIGHTS

botCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))

topCircle :: Color -> Picture
topCircle c = colored c (translated 0   1.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True  = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red   & frame

trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 2 == 0 = trafficLight True
  | otherwise                = trafficLight False

exercise1 :: IO ()
exercise1 = animationOf trafficController


-- BLOSSOMS

tree :: Int -> Picture
tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))

exercise2 :: IO ()
exercise2 = drawingOf (tree 5)


-- SOKOBAN

wall :: Picture
wall = blank

ground :: Picture
ground = blank

storage :: Picture
storage = blank

box :: Picture
box = blank

drawTile :: Int -> Picture
drawTile i = blank

maze :: Int -> Int -> Int
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

pictureOfMaze :: Picture
pictureOfMaze = blank

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
