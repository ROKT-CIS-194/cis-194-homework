{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main :: IO ()
main = exercise1

-- LAST WEEK
-- Replace with your implementation from last week.
data Coord = C Int Int deriving (Eq, Ord, Show)

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq, Show)

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

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box = box
drawTile _ = blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 10  || abs y > 10  = Blank
  | abs x == 10 || abs y == 10 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

drawAt :: Coord -> Picture -> Picture
drawAt (C x y) p = translated (fromIntegral x) (fromIntegral y) p

drawTileAt :: Int -> Int -> Picture
drawTileAt x y = drawAt (C x y) $ drawTile (maze (C x y))

pictureOfMaze :: Picture
pictureOfMaze = foldr (&) blank $ map (\(x,y) -> drawTileAt x y) [(x,y) | x <- [-10..10], y <- [-10..10]]


-- #1 MOVEMENT
data Direction = U | D | L | R

type World = Coord

moveIn :: Direction -> Coord -> Coord
moveIn U (C x y) = C  x   (y+1)
moveIn D (C x y) = C  x   (y-1)
moveIn L (C x y) = C (x-1) y
moveIn R (C x y) = C (x+1) y

moveIfAllowed :: Direction -> Coord -> Coord
moveIfAllowed d c
  | allowed (maze c') = c'
  | otherwise = c
  where c' = moveIn d c
        allowed t
          | (t == Ground || t == Storage) = True
          | otherwise = False

player :: Picture
player = solidCircle 0.5

stepInput :: Event -> World -> World
stepInput (KeyPress k) c
  | k == "Up"    = moveIfAllowed U c
  | k == "Down"  = moveIfAllowed D c
  | k == "Left"  = moveIfAllowed L c
  | k == "Right" = moveIfAllowed R c
stepInput _ w = w

exercise1 :: IO ()
exercise1 = interactionOf world0 stepTime stepInput output
  where
    world0 :: World
    world0 = C 0 1

    stepTime :: Double -> World -> World
    stepTime _ = id

    output :: World -> Picture
    output c = drawAt c player & pictureOfMaze


-- #2 LOOK THE RIGHT WAY

data World2 = World2 Coord Direction

arrow :: Picture
arrow = path [(-0.5,0),(0.5,0)] & path [(0.5,0),(0.3,0.3)] & path [(0.5,0),(0.3,-0.3)]

player2 :: Direction -> Picture
player2 U = rotated (pi/2) arrow
player2 D = rotated (3*pi/2) arrow
player2 L = rotated (pi) arrow
player2 R = rotated 0 arrow

stepInput2 :: Event -> World2 -> World2
stepInput2 (KeyPress k) (World2 c _)
  | k == "Up"    = World2 (moveIfAllowed U c) U
  | k == "Down"  = World2 (moveIfAllowed D c) D
  | k == "Left"  = World2 (moveIfAllowed L c) L
  | k == "Right" = World2 (moveIfAllowed R c) R
stepInput2 _ w = w

exercise2 :: IO ()
exercise2 = interactionOf world0 stepTime stepInput2 output
  where
    world0 :: World2
    world0 = World2 (C 0 1) R

    stepTime :: Double -> World2 -> World2
    stepTime _ = id

    output :: World2 -> Picture
    output (World2 c d) = drawAt c (player2 d) & pictureOfMaze


-- #3 RESET!

resetableInteractionOf
  :: world
  -> (Double -> world -> world)
  -> (Event -> world -> world)
  -> (world -> Picture)
  -> IO ()
resetableInteractionOf world0 stepTime stepInput output
  = interactionOf world0 stepTime stepInput' output
  where stepInput' (KeyPress k) _ | k == "Esc" = world0
        stepInput' e s = stepInput e s


exercise3 :: IO ()
exercise3 = resetableInteractionOf world0 stepTime stepInput2 output
  where
    world0 :: World2
    world0 = World2 (C 0 1) R

    stepTime :: Double -> World2 -> World2
    stepTime _ = id

    output :: World2 -> Picture
    output (World2 c d) = drawAt c (player2 d) & pictureOfMaze


-- #4 NEW LEVELS

maze2 :: Coord -> Tile
maze2 (C _ _) = Blank
