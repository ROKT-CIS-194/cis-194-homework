{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week2 where

import CodeWorld

-- LAST WEEK
-- Replace with your implementation from last week.

data Coord = C Int Int deriving (Eq, Ord, Show)

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq, Show)

wall :: Picture
wall = coloured (RGBA 0.8 0.1 0.2 1.0) $ solidRectangle 1 1

ground :: Picture
ground = coloured (RGBA 0.9 0.8 0.7 1.0) $ solidRectangle 1 1

storage :: Picture
storage = (coloured green $ solidCircle 0.1) <> ground

box :: Picture
box = coloured brown $ solidRectangle 1 1

drawTile :: Tile -> Picture
drawTile i = case i of
  Wall -> wall
  Ground -> ground
  Storage -> storage
  Box -> box
  _ -> blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

pictureOfMaze :: Picture
pictureOfMaze = pictures tiles
  where tiles = do x <- [-10..10]
                   y <- [-10..10]
                   return . trans x y . drawTile . maze $ (C x y)
        trans x y = translated (fromIntegral x) (fromIntegral y)

-- #1 MOVEMENT

data World = World Coord

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

player :: Picture
player = coloured black $ pictures p
  where p = [solidCircle 0.2,
             path [(0.0,0.0),(0.3,0.4)],
             path [(0.0,0.0),(-0.3,0.4)]]

exercise1 :: IO ()
exercise1 = interactionOf world0 stepTime stepInput output
  where
    world0 = World (C 1 1)

    stepTime :: Double -> World -> World
    stepTime _ = id

    stepInput :: Event -> World -> World
    stepInput (KeyPress key) (World c1) = World c2
      where c2 = case key of
              "Up" -> adjacentCoord U c1
              "Down" -> adjacentCoord D c1
              "Left" -> adjacentCoord L c1
              "Right" -> adjacentCoord R c1
              _ -> c1

    output :: World -> Picture
    output (World (C x y)) = (translated (fromIntegral x) (fromIntegral y) $ player) <> pictureOfMaze


-- #2 LOOK THE RIGHT WAY

data Direction = U | D | L | R deriving (Eq, Show)

player2 :: Direction -> Picture
player2 _ = blank

exercise2 :: IO ()
exercise2 = interactionOf world0 stepTime stepInput output
  where
    world0 = World (C 1 1)

    stepTime :: Double -> World -> World
    stepTime _ = id

    stepInput :: Event -> World -> World
    stepInput _ = id

    output :: World -> Picture
    output _ = blank


-- #3 RESET!

resetableInteractionOf
  :: world
  -> (Double -> world -> world)
  -> (Event -> world -> world)
  -> (world -> Picture)
  -> IO ()
resetableInteractionOf _ _ _ _ =
  return ()

exercise3 :: IO ()
exercise3 = resetableInteractionOf world0 stepTime stepInput output
  where
    world0 = World (C 1 1)

    stepTime :: Double -> World -> World
    stepTime _ = id

    stepInput :: Event -> World -> World
    stepInput _ = id

    output :: World -> Picture
    output _ = blank


-- #4 NEW LEVELS

maze2 :: Coord -> Tile
maze2 (C _ _) = Blank
