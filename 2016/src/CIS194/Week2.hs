{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week2 where

import CodeWorld

-- LAST WEEK
-- Replace with your implementation from last week.

data Coord = C Int Int deriving (Eq, Ord, Show)

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq, Show)

wall :: Picture
wall = blank

ground :: Picture
ground = blank

storage :: Picture
storage = blank

box :: Picture
box = blank

drawTile :: Tile -> Picture
drawTile _ = blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

pictureOfMaze :: Picture
pictureOfMaze = blank


-- #1 MOVEMENT

type World = ()

player :: Picture
player = blank

exercise1 :: IO ()
exercise1 = interactionOf world0 stepTime stepInput output
  where
    world0 :: World
    world0 = ()

    stepTime :: Double -> World -> World
    stepTime _ = id

    stepInput :: Event -> World -> World
    stepInput _ = id

    output :: World -> Picture
    output _ = blank


-- #2 LOOK THE RIGHT WAY

data Direction = U | D | L | R deriving (Eq, Show)

player2 :: Direction -> Picture
player2 _ = blank

exercise2 :: IO ()
exercise2 = interactionOf world0 stepTime stepInput output
  where
    world0 :: World
    world0 = ()

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
    world0 :: World
    world0 = ()

    stepTime :: Double -> World -> World
    stepTime _ = id

    stepInput :: Event -> World -> World
    stepInput _ = id

    output :: World -> Picture
    output _ = blank


-- #4 NEW LEVELS

maze2 :: Coord -> Tile
maze2 (C _ _) = Blank
