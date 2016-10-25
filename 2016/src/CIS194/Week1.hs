{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week1 where

import CodeWorld
import Data.Fixed

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

tree :: Double -> Int -> Picture
tree t 0 = (coloured colour (curve [(0,0),(0.2,0.5),(0,1)])) &
           (coloured colour (curve [(0,0),(-0.2,0.5),(0,1)]))
           where cols = [(1,0),(0.7,1),(0,1),(1,0.9)]
                 lerp = \f -> ((f (cols !! (floor t `mod` 4))) * (1 - (mod' t 1))) +
                              ((f (cols !! (floor (t+1) `mod` 4))) * (mod' t 1))
                 colour = (RGBA (lerp fst) (lerp snd) 0 1)
tree t n = path [(0,0),(0,1)] &
  translated 0 1 $
  (rotated (angle t) . tree t $ (n-1)) &
  (rotated (- (angle (t+pi/1.5))) . tree t $ n-1)
  where angle = \t -> pi/10/(((sin t) * 0.1) + 0.95)

exercise2 :: IO ()
exercise2 = animationOf (\t -> (tree t 5))


-- SOKOBAN

wall :: Picture
wall = coloured (RGBA 0.8 0.1 0.2 1.0) $ solidRectangle 1 1

ground :: Picture
ground = coloured (RGBA 0.9 0.8 0.7 1.0) $ solidRectangle 1 1

storage :: Picture
storage = (coloured green $ solidCircle 0.1) <> ground

box :: Picture
box = coloured brown $ solidRectangle 1 1

drawTile :: Int -> Picture
drawTile i = case i of
  1 -> wall
  2 -> ground
  3 -> storage
  4 -> box
  _ -> blank

maze :: Int -> Int -> Int
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

pictureOfMaze :: Picture
pictureOfMaze = pictures tiles
  where tiles = do x <- [-10..10]
                   y <- [-10..10]
                   return . translated (fromIntegral x) (fromIntegral y) . drawTile $ maze x y

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
