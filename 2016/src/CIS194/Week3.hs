{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week3 where

import Control.Applicative
import Control.Monad (guard)
import CodeWorld

-- Lists

data List a
  = Empty
  | Entry a (List a)
    deriving (Eq, Ord, Show)

instance Foldable List where
  foldMap _ Empty = mempty
  foldMap f (Entry x xs) = f x `mappend` foldMap f xs

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

concatLists :: List (List a) -> List a
concatLists Empty = Empty
concatLists (Entry Empty ys) = concatLists ys
concatLists (Entry (Entry x xs) ys) = Entry x (concatLists (Entry xs ys))


-- Coordinates

data Coord
  = C Integer Integer
    deriving (Eq, Ord, Show)

data Direction
  = R | U | L | D
    deriving (Eq, Ord, Show)

eqCoord :: Coord -> Coord -> Bool
eqCoord = undefined

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo = undefined


-- The maze

data Tile
  = Wall | Ground | Storage | Box | Blank
    deriving (Eq, Ord, Show)

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c = if maze c == Box then Ground else maze c

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty c = noBoxMaze c
mazeWithBoxes (Entry c _) c' | c == c' = Box
mazeWithBoxes (Entry _ cs) c = mazeWithBoxes cs c


-- The state

data State = State
  { playerPos :: Coord
  , playerDir :: Direction
  , gameBoxes :: List Coord
  } deriving Show

allCoords :: List Coord
allCoords = go (-10) (-10)
  where
    go 11 10 = Empty
    go 11 y = go (-10) (y+1)
    go x y = Entry (C x y) (go (x+1) y)

initialBoxes :: List Coord
initialBoxes = concatLists (mapList (\c -> f (maze c) c) allCoords)
  where
    f t c = if t == Box then Entry c Empty else Empty

initialState :: State
initialState = State (C 0 1) R initialBoxes


-- Event handling

nextPos :: (Coord -> Tile) -> List Coord -> Direction -> Coord -> Maybe Coord
nextPos mz boxes dir (C x y) =
  pos <$ guard (mz pos `elem` [Ground, Storage]) <* guard (pos `notElem` boxes)
  where
    pos = case dir of
      U -> C x (y+1)
      D -> C x (y-1)
      L -> C (x-1) y
      R -> C (x+1) y

shiftBoxes
  :: (Coord -> Tile) -> Direction -> Coord -> List Coord -> Maybe (List Coord)
shiftBoxes mz dir p boxes = go boxes
  where
    go Empty = Just Empty
    go (Entry q cs)
      | p == q = Entry <$> nextPos mz boxes dir q <*> go cs
      | otherwise = Entry q <$> go cs

handleEvent :: Event -> State -> State
handleEvent ev s@(State pos _ boxes) =
  maybe s (\d -> maybe (State pos d boxes) id (go d)) dirMay
  where
    go d = do
      p <- nextPos noBoxMaze Empty d pos <|> Just pos
      boxes' <- shiftBoxes noBoxMaze d p boxes
      return (State p d boxes')
    dirMay = case ev of
      KeyPress "Up"    -> Just U
      KeyPress "Down"  -> Just D
      KeyPress "Left"  -> Just L
      KeyPress "Right" -> Just R
      _                -> Nothing


-- Drawing

circ, square :: Color -> Picture
circ col = coloured col (solidCircle 0.35)
square col = coloured col (solidRectangle 1.02 1.02)

drawTile :: Tile -> Picture
drawTile tile =
  case tile of
    Wall    -> square (grey 0.4)
    Ground  -> square yellow
    Storage -> circ white & square yellow
    Box     -> square brown
    Blank   -> blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

player :: Direction -> Picture
player dir =
  rotated (case dir of L -> pi; R -> 0; U -> pi/2; D -> 3*pi/2)
  $ translated 0.1 0.1 (scaled 0.2 0.2 (circ white))
    & translated 0.1 (-0.1) (scaled 0.2 0.2 (circ white))
    & circ black

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

draw :: State -> Picture
draw (State pos dir boxes) =
  atCoord pos (player dir) & pictureOfBoxes boxes & pictureOfMaze


-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction initialState (\_ c -> c) handleEvent draw


-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw


-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s


-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s


-- The main function

main :: IO ()
main = runInteraction (withStartScreen $ resetable sokoban)
