{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week2 where

import CodeWorld
import Data.Text

-- LAST WEEK
-- Replace with your implementation from last week.

data Coord = C Double Double deriving (Eq, Ord, Show)

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

maze1 :: Coord -> Tile
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

pictureOfMaze :: (Coord -> Tile) -> Picture
pictureOfMaze maze = pictures tiles
  where tiles = do x <- [-10..10]
                   y <- [-10..10]
                   return . translated x y . drawTile . maze $ (C x y)

-- #1 MOVEMENT

data World = World { playerCoord :: Coord
                   , lastPlayerCoord :: Coord
                   , playerDirection :: Direction
                   , lastMoveTime :: Double
                   , worldTime :: Double
                   }

adjacentCoord :: Coord -> Direction -> Coord
adjacentCoord (C x y) R = C (x+1) y
adjacentCoord (C x y) U = C  x   (y+1)
adjacentCoord (C x y) L = C (x-1) y
adjacentCoord (C x y) D = C  x   (y-1)

moveToCoord :: (Coord -> Tile) -> Coord -> Direction -> Coord
moveToCoord maze c d
  | canMove = adjacent
  | otherwise = c
  where adjacent = adjacentCoord c d
        canMove = case (maze adjacent) of
          Wall -> False
          Ground -> True
          Storage -> True
          Box -> False
          _ -> False

player :: Picture
player = coloured black $ pictures p
  where p = [solidCircle 0.2,
             path [(0.0,0.0),(0.3,0.4)],
             path [(0.0,0.0),(-0.3,0.4)]]

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated x y

lerpCoords :: Coord -> Coord -> Double -> Coord
lerpCoords (C x1 y1) (C x2 y2) t = C x' y'
  where t' | t < 0.0 = 0.0
           | t > 1.0 = 1.0
           | otherwise = t
        x' = x1 + ((x2 - x1) * t')
        y' = y1 + ((y2 - y1) * t')

exercise1 :: IO ()
exercise1 = interactionOf world0 stepTime stepInput output
  where
    startCoord = C (-3) 3
    world0 = World {playerCoord = startCoord, lastPlayerCoord = startCoord,
                    playerDirection = U, lastMoveTime = 0.0, worldTime = 0.0}

    maze = maze1

    stepTime :: Double -> World -> World
    stepTime t w = w { worldTime = newWorldTime }
      where newWorldTime = worldTime w + t

    stepInput :: Event -> World -> World
    stepInput (KeyPress key) w
      | moveTimeDiff >= 0.1 = w { playerCoord = c2
                                 ,  lastPlayerCoord = c1
                                 , lastMoveTime = worldTime w }
      | otherwise = w
      where moveTimeDiff = (worldTime w) - (lastMoveTime w)
            moveToCoord' = moveToCoord maze c1
            c1 = playerCoord w
            c2 = case key of
              "Up" -> moveToCoord' U
              "Down" -> moveToCoord' D
              "Left" -> moveToCoord' L
              "Right" -> moveToCoord' R
              _ -> c1
    stepInput _ w = w

    output :: World -> Picture
    output w = (atCoord c $ player) <> pictureOfMaze maze1
      where a = lastPlayerCoord w
            b = playerCoord w
            t = ((worldTime w) - (lastMoveTime w)) * 10
            c = lerpCoords a b t


-- #2 LOOK THE RIGHT WAY

data Direction = U | D | L | R deriving (Eq, Show)

player2 :: Direction -> Picture
player2 _ = blank

exercise2 :: IO ()
exercise2 = interactionOf world0 stepTime stepInput output
  where
    startCoord = C (-9) (-9)
    world0 = World {playerCoord = startCoord, lastPlayerCoord = startCoord,
                    playerDirection = U, lastMoveTime = 0.0, worldTime = 0.0}


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
    startCoord = C (-9) (-9)
    world0 = World {playerCoord = startCoord, lastPlayerCoord = startCoord,
                    playerDirection = U, lastMoveTime = 0.0, worldTime = 0.0}


    stepTime :: Double -> World -> World
    stepTime _ = id

    stepInput :: Event -> World -> World
    stepInput _ = id

    output :: World -> Picture
    output _ = blank


-- #4 NEW LEVELS

maze2 :: Coord -> Tile
maze2 (C _ _) = Blank
