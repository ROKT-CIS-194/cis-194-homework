{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main :: IO ()
main = exercise1

-- TRAFFIC LIGHTS

trafficLight :: Color -> Double ->  Picture
trafficLight c y = colored c (translated 0 y (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

lightColor :: Bool -> Color -> Color
lightColor True c = c
lightColor False _ = black

trafficLights :: Bool -> Bool -> Bool -> Picture
trafficLights r a g =
  frame &
  trafficLight (lightColor r red)    2.5 &
  trafficLight (lightColor a orange) 0   &
  trafficLight (lightColor g green) (-2.5)

trafficController :: Double -> Picture
trafficController t
  | t' <= 1 = trafficLights False False True
  | t' <= 2 = trafficLights False True False
  | t' <= 4 = trafficLights True False False
  | otherwise = trafficLights True True False
  where t' = (round t `mod` 6)

exercise1 :: IO ()
exercise1 = animationOf trafficController


-- BLOSSOMS

tree :: Picture -> Int -> Picture
tree b 0 = b
tree b n = path [(0,0),(0,1)] &
           translated 0 1 (rotated (pi/10) (tree b (n-1)) &
                           rotated (- pi/10) (tree b (n-1)))

blossom :: Double -> Picture
blossom t = colored pink (solidCircle s)
  where s = case () of
              _ | t <= 10 -> t/30
                | otherwise -> (1/3)

treeAnimation :: Double -> Picture
treeAnimation t = tree (blossom t) 8

exercise2 :: IO ()
exercise2 = animationOf treeAnimation


-- SOKOBAN

square :: Picture
square = (solidRectangle 1 1)

wall :: Picture
wall =  colored (gray 0.5) square

ground :: Picture
ground = colored yellow square

storage :: Picture
storage = solidCircle 0.25 & ground

box :: Picture
box = colored orange square

drawTile :: Int -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

maze :: Int -> Int -> Int
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

drawTileAt :: Int -> Int -> Picture
drawTileAt x y = translated (fromIntegral x) (fromIntegral y) $ drawTile (maze x y)

pictureOfMaze :: Picture
pictureOfMaze = foldr (&) blank $ map (\(x,y) -> drawTileAt x y) [(x,y) | x <- [-10..10], y <- [-10..10]]

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
