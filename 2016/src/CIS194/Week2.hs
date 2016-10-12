{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module CIS194.Week2 where

import CodeWorld
import Control.Applicative
import Control.Arrow
import Control.Lens hiding ((&))
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Maybe

import CIS194.FRP

data Coord = C Int Int deriving (Eq, Ord, Show)

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq, Show)

data Direction = U | D | L | R deriving (Eq, Show)

circ, square :: Color -> Picture
circ col = coloured col (solidCircle 0.35)
square col = coloured col (solidRectangle 1.02 1.02)

drawTile :: Tile -> Picture
drawTile tile = case tile of
  Wall -> square (grey 0.4)
  Ground -> square yellow
  Storage -> circ white & square yellow
  Box -> square brown
  Blank -> blank

type Maze = (Direction, Coord, Coord -> Tile)

pictureOfMaze :: Maze -> Picture
pictureOfMaze (_, _, mz) =
  mconcat [f x y | x <- [-10..10], y <- [-10..10]]
  where
    f x y =
      let (tx,ty) = (fromIntegral x, fromIntegral y)
      in translated tx ty (drawTile (mz (C x y)))


-- #1 MOVEMENT

type World = (Picture, FRP Identity (Either Double Event) Picture)

data GameState
  = GameState
    { _gamePlayerPos :: Coord
    , _gamePlayerDir :: Direction
    }
makeLenses ''GameState

getPos :: MonadState GameState m => FRP m i Coord
getPos = liftMT (\_ -> use gamePlayerPos)

setPos :: MonadState GameState m => FRP m Coord Coord
setPos = liftMT (\x -> x <$ assign gamePlayerPos x)

setDir :: MonadState GameState m => FRP m Direction Direction
setDir = liftMT (\x -> x <$ assign gamePlayerDir x)


player :: Direction -> Picture
player dir =
  rotated (case dir of L -> pi; R -> 0; U -> pi/2; D -> 3*pi/2)
  $ translated 0.1 0.1 (scaled 0.2 0.2 (circ white))
    & translated 0.1 (-0.1) (scaled 0.2 0.2 (circ white))
    & circ black

render :: (Maze, GameState) -> Picture
render (mz, GameState (C x y) dir) =
  translated (fromIntegral x) (fromIntegral y) (player dir) & pictureOfMaze mz


parseKey :: Monad m => FRP (MaybeT m) Event Direction
parseKey = proc event ->
  case event of
    KeyPress "Up" -> returnA -< U
    KeyPress "Down" -> returnA -< D
    KeyPress "Left" -> returnA -< L
    KeyPress "Right" -> returnA -< R
    _ -> empty -< ()

checkMovement :: MonadReader Maze m => FRP (MaybeT m) (Coord, Direction) Coord
checkMovement = proc (C x y, dir) -> do
  let nextPos = case dir of
        U -> C x (y+1)
        D -> C x (y-1)
        L -> C (x-1) y
        R -> C (x+1) y
  mz <- liftMT (const (view _3)) -< ()
  case mz nextPos of
    Ground -> returnA -< nextPos
    Storage -> returnA -< nextPos
    _ -> empty -< ()

game :: (Monad m)
     => Maze -> GameState
     -> FRP m (Either Double Event) (Maze, GameState)
game mz st = run $ proc inp -> do
  case inp of
    Left _ -> returnA -< ()
    Right event -> do
      pos <- getPos -< ()
      maskMaybeMT
        (setPos <<< checkMovement <<< second parseKey)
        -< (pos, event)
      returnA -< ()
  where
    run = fmap (mz,) . execStateMT st . runReaderMT mz

exercise1 :: IO ()
exercise1 = interactionOf world0 stepTime stepInput fst
  where
    state0 = GameState (maze ^. _2) (maze ^. _1)
    world0 = (blank, fmap render (game maze state0))
    stepTime dt (_, Step m) = runIdentity (m (Left dt))
    stepInput ev (_, Step m) = runIdentity (m (Right ev))


-- #2 LOOK THE RIGHT WAY

game2 :: (Monad m)
      => Maze -> GameState
      -> FRP m (Either Double Event) (Maze, GameState)
game2 mz st = run $ proc inp -> do
  case inp of
    Left _ -> returnA -< ()
    Right event -> do
      pos <- getPos -< ()
      maskMaybeMT
        (setPos <<< checkMovement <<< second (setDir <<< parseKey))
        -< (pos, event)
      returnA -< ()
  where
    run = fmap (mz,) . execStateMT st . runReaderMT mz

exercise2 :: IO ()
exercise2 = interactionOf world0 stepTime stepInput fst
  where
    state0 = GameState (maze ^. _2) (maze ^. _1)
    world0 = (blank, fmap render (game2 maze state0))
    stepTime dt (_, Step m) = runIdentity (m (Left dt))
    stepInput ev (_, Step m) = runIdentity (m (Right ev))


-- #3 RESET!

resetableInteractionOf
  :: world
  -> (Double -> world -> world)
  -> (Event -> world -> world)
  -> (world -> Picture)
  -> IO ()
resetableInteractionOf world0 ft fe fp =
  interactionOf world0 ft fe' fp
  where
    fe' ev w = case ev of
      KeyPress "Esc" -> world0
      _ -> fe ev w

exercise3 :: IO ()
exercise3 = resetableInteractionOf world0 stepTime stepInput fst
  where
    state0 = GameState (maze ^. _2) (maze ^. _1)
    world0 = (blank, fmap render (game2 maze state0))
    stepTime dt (_, Step m) = runIdentity (m (Left dt))
    stepInput ev (_, Step m) = runIdentity (m (Right ev))


-- #4 NEW LEVELS

maze :: Maze
maze = (R, C 0 1, go)
  where
    go (C x y) = case xs !! ((x+10) + (((-y)+10) * 21)) of
      '.' -> Blank
      ' ' -> Ground
      '#' -> Wall
      'o' -> Storage
      'B' -> Box
      c -> error ("Unknown cell type: " ++ show c)
    xs =
      ".....................\
      \.....................\
      \.....................\
      \.....................\
      \.....................\
      \.....###########.....\
      \.....#         #.....\
      \.....#         #.....\
      \.....#   #######.....\
      \.....#        o#.....\
      \.....########  #.....\
      \.....#         #.....\
      \.....# #####   #.....\
      \.....# #####   ###...\
      \.....#o       BB #...\
      \.....####### # # #...\
      \...........#     #...\
      \...........#######...\
      \.....................\
      \.....................\
      \....................."
