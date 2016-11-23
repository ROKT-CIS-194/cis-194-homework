{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week2 where

import CodeWorld
import Data.Text (pack)
import Data.Maybe

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
                   , lastPlayerDirection :: Direction
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

clampT :: Double -> Double
clampT t | t < 0.0 = 0.0
         | t > 1.0 = 1.0
         | otherwise = t

lerpCoords :: Coord -> Coord -> Double -> Coord
lerpCoords (C x1 y1) (C x2 y2) t = C x' y'
  where t' = clampT t
        x' = x1 + ((x2 - x1) * t')
        y' = y1 + ((y2 - y1) * t')

slerpDirection :: Direction -> Direction -> Double -> Double
slerpDirection d1 d2 t = theta
  where direction d = case d of
          U -> [0.0, 1.0]
          R -> [1.0, 0.0]
          D -> [0.0, (-1.0)]
          L -> [(-1.0), 0.0]
        v1 = direction d1
        v2 = direction d2
        t' = clampT t
        omega = 1.0
        v1' = map (\e -> ((sin ((1 - t') * omega)) / (sin omega)) * e) v1
        v2' = map (\e -> ((sin (t' * omega)) / (sin omega)) * e) v2
        v3 = zipWith (+) v1' v2'
        theta = (atan2 (v3 !! 1) (v3 !! 0)) - (atan2 1.0 0.0)

stepInput :: (Coord -> Tile) -> Event -> World -> World
stepInput maze (KeyPress key) w
  | moveTimeDiff >= 0.1 = w { playerCoord = c2
                            , playerDirection = (fromMaybe d1 d2)
                            , lastPlayerDirection = d1
                            , lastPlayerCoord = c1
                            , lastMoveTime = worldTime w }
  | otherwise = w
  where moveTimeDiff = (worldTime w) - (lastMoveTime w)
        moveToCoord' = moveToCoord maze c1
        d1 = playerDirection w
        d2 = case key of
          "Up" -> Just U
          "Down" -> Just D
          "Left" -> Just L
          "Right" -> Just R
          _ -> Nothing
        c1 = playerCoord w
        c2 = maybe c1 moveToCoord' d2
stepInput _ _ w = w

stepTime :: Double -> World -> World
stepTime t w = w { worldTime = newWorldTime }
  where newWorldTime = worldTime w + t

output :: Picture -> World -> Picture
output player w = (atCoord c . rotated theta $ player) <> pictureOfMaze maze1
  where c1 = lastPlayerCoord w
        c2 = playerCoord w
        d1 = lastPlayerDirection w
        d2 = playerDirection w
        t = ((worldTime w) - (lastMoveTime w)) * 10
        c = lerpCoords c1 c2 t
        theta = slerpDirection d1 d2 t

initWorld :: Coord -> World
initWorld startCoord = World {playerCoord = startCoord, lastPlayerCoord = startCoord,
                              playerDirection = R, lastPlayerDirection = R,
                              lastMoveTime = 0.0, worldTime = 0.0}

exercise1 :: IO ()
exercise1 = interactionOf world0 stepTime (stepInput maze1) (output player)
  where
    world0 = initWorld $ C (-3) 3

-- #2 LOOK THE RIGHT WAY

data Direction = U | D | L | R deriving (Eq, Show)

exercise2 :: IO ()
exercise2 = interactionOf world0 stepTime (stepInput maze1)
            (output $ player)
  where
    startCoord = C (-3) (-3)
    world0 = initWorld $ C (-3) 3

-- #3 RESET!

resetableInteractionOf
  :: world
  -> (Double -> world -> world)
  -> (Event -> world -> world)
  -> (world -> Picture)
  -> IO ()
resetableInteractionOf w stepTime stepInput output =
  interactionOf w stepTime stepInput' output
  where stepInput' (KeyPress k) _ | k == "Esc" = w
        stepInput' e w = stepInput e w

exercise3 :: IO ()
exercise3 = resetableInteractionOf world0 stepTime (stepInput maze1)
            (output $ player)
  where
    startCoord = C (-3) (-3)
    world0 = initWorld $ C (-3) 3

-- #4 NEW LEVELS

maze2 :: Coord -> Tile
maze2 (C _ _) = Blank
