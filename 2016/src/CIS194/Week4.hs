{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week4 where

import CodeWorld
import Control.Applicative
import Control.Monad (guard)
import Control.Monad.Trans.Maybe
import Data.Functor.Identity

-- Coordinates

data Coord = C Integer Integer deriving (Eq, Show)

data Direction = R | U | L | D deriving (Eq, Show)

eqCoord :: Coord -> Coord -> Bool
eqCoord = undefined

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo = undefined

-- Exercise 1: Lists

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

elemList :: Eq a => a -> List a -> Bool
elemList _ Empty = False
elemList x (Entry y ys) = x == y || elemList x ys

appendList :: List a -> List a -> List a
appendList Empty ys = ys
appendList (Entry x xs) ys = Entry x (appendList xs ys)

concatLists :: List (List a) -> List a
concatLists Empty = Empty
concatLists (Entry Empty ys) = concatLists ys
concatLists (Entry (Entry x xs) ys) = Entry x (concatLists (Entry xs ys))

listLength :: List a -> Integer
listLength Empty = 0
listLength (Entry _ xs) = 1 + listLength xs

filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList f (Entry x xs) =
  if f x then Entry x (filterList f xs) else (filterList f xs)

nth :: List a -> Integer -> a
nth Empty _ = error "nth: list too short"
nth (Entry x xs) i = case compare i 0 of
  LT -> error "nth: i<0"
  EQ -> x
  GT -> nth xs (i - 1)

-- Exercise 2: Graph Search

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = go Empty (Entry initial Empty)
  where
    go _ Empty = True
    go seen (Entry x xs)
      | elemList x seen = go seen xs
      | otherwise = isOk x && go (Entry x seen) (adjacent x)

-- Exercise 3: Check closedness of mazes

isClosed :: Maze -> Bool
isClosed (Maze pos mz) = mz pos /= Box && isGraphClosed pos adjacent isOk
  where
    isOk x = mz x `elemList` Entry Ground (Entry Storage (Entry Box Empty))
    adjacent x =
      appendList (guardTile (adjacentCoord U x)) $
      appendList (guardTile (adjacentCoord D x)) $
      appendList (guardTile (adjacentCoord L x)) $
                 (guardTile (adjacentCoord R x))
    isWall t = elemList t (Entry Wall (Entry Blank Empty))
    guardTile x = if isWall (mz x) then Empty else Entry x Empty

pictureOfBools :: List Bool -> Picture
pictureOfBools xs = translated (-fromIntegral k /2) (fromIntegral k) (go 0 xs)
  where n = listLength xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ Empty = blank
        go i (Entry b bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

exercise3 :: IO ()
exercise3 = drawingOf (pictureOfBools (mapList isClosed extraMazes))

-- Exercise 4: Multi-Level Sokoban
-- Extend your game from last week (or the code from the lecture) to implement
-- multi-level sokoban.

-- The state

data State = State
  { playerPos  :: Coord
  , playerDir  :: Direction
  , gameBoxes  :: List Coord
  } deriving Eq

allCoords :: List Coord
allCoords = go (-10) (-10)
  where
    go 11 10 = Empty
    go 11 y = go (-10) (y+1)
    go x y = Entry (C x y) (go (x+1) y)

initialBoxes :: Maze -> List Coord
initialBoxes (Maze _ mz) = concatLists (mapList (\c -> f (mz c) c) allCoords)
  where
    f t c = if t == Box then Entry c Empty else Empty

initialState :: Maze -> State
initialState mz = State (C 0 1) R (initialBoxes mz)

noBoxMaze :: Maze -> Coord -> Tile
noBoxMaze (Maze _ mz) c = if mz c == Box then Ground else mz c

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

handleEvent :: (Alternative m, Monad m) => Maze -> Event -> State -> m State
handleEvent mz ev s@(State pos _ boxes) =
  maybe (return s) (\d -> maybe (return (State pos d boxes)) return (go d)) dirMay
  where
    go d = do
      p <- nextPos (noBoxMaze mz) Empty d pos <|> Just pos
      boxes' <- shiftBoxes (noBoxMaze mz) d p boxes
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

pictureOfMaze :: Maze -> Picture
pictureOfMaze mz = draw21times (\r -> draw21times (\c -> drawTileAt mz (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Maze -> Coord -> Picture
drawTileAt mz c = atCoord c (drawTile (noBoxMaze mz c))

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

drawState :: Maze -> State -> Picture
drawState mz (State pos dir boxes) =
  atCoord pos (player dir) & pictureOfBoxes boxes & (pictureOfMaze mz)


-- The complete interaction

sokoban :: (Alternative m, Monad m) => Maze -> Interaction m State
sokoban mz@(Maze _ f) =
  Interaction (initialState mz) checkDone (handleEvent mz) (drawState mz)
  where
    checkDone _ s@(State _ _ boxes) =
      s <$ guard (any (\x -> f x /= Storage) boxes)

-- The general interaction type

data Interaction m world = Interaction
        world
        (Double -> world -> m world)
        (Event -> world -> m world)
        (world -> Picture)

runInteraction :: Interaction Identity s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf
      state0
      (\t st -> runIdentity (step t st))
      (\e st -> runIdentity (handle e st))
      draw

-- Resetable interactions

resetable :: Monad m => Interaction m s -> Interaction m s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = return state0
        handle' e s = handle e s

-- Levels

data LState m world
  = AllDone
  | Levels
      (List Maze)
      world
      (Double -> world -> MaybeT m world)
      (Event -> world -> MaybeT m world)
      (world -> Picture)

withLevels
  :: Monad m
  => List Maze -> (Maze -> Interaction (MaybeT m) s) -> Interaction m (LState m s)
withLevels allMazes f = case allMazes of
  Empty ->
    Interaction AllDone (const return) (const return) drawDone
  Entry mz mzs -> case f mz of
    Interaction s step handle draw ->
      Interaction (Levels mzs s step handle draw) stepGo handleGo drawGo
  where
    drawDone _ = text "All Done!"

    stepGo _ AllDone = return AllDone
    stepGo t (Levels mzs s step handle draw) = do
      sM' <- runMaybeT (step t s)
      case sM' of
        Just s' -> return (Levels mzs s' step handle draw)
        Nothing -> nextLevel mzs

    handleGo _ AllDone = return AllDone
    handleGo e (Levels mzs s step handle draw) = do
      sM' <- runMaybeT (handle e s)
      case sM' of
        Just s' -> return (Levels mzs s' step handle draw)
        Nothing -> nextLevel mzs

    drawGo AllDone = drawDone AllDone
    drawGo (Levels _ s _ _ draw) = draw s

    nextLevel mzs = case mzs of
      Empty ->
        return AllDone
      Entry mz mzs' -> do
        let Interaction s' fstep' fhandle' fdraw' = f mz
        return (Levels mzs' s' fstep' fhandle' fdraw')

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world deriving Eq

withStartScreen :: Monad m => Interaction m s  -> Interaction m (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = return StartScreen
    step' t (Running s) = Running <$> step t s

    handle' (KeyPress key) StartScreen | key == " " = return (Running state0)
    handle' _              StartScreen              = return StartScreen
    handle' e              (Running s)              = Running <$> handle e s

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

-- Undoable interactions

-- We need to remember the current state, and all past states:

data WithUndo a = WithUndo a (List a)

withUndo :: (Eq a, Monad m) => Interaction m a -> Interaction m (WithUndo a)
withUndo (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    draw' (WithUndo s _) = draw s

    state0' = WithUndo state0 Empty

    step' t (WithUndo s stack) = WithUndo <$> step t s <*> pure stack

    handle' (KeyPress key) (WithUndo s stack) | key == "U" =
      case stack of
        Entry s' stack' -> return (WithUndo s' stack')
        Empty -> return (WithUndo s Empty)
    handle' e (WithUndo s stack) = do
      s' <- handle e s
      return (if s' == s then WithUndo s stack else WithUndo s' (Entry s stack))

exercise4 :: IO ()
exercise4 =
  runInteraction
  . withStartScreen
  . withLevels mazes
  $ resetable . withUndo . sokoban

main :: IO ()
main = return ()


-- MAZES

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

data Maze = Maze Coord (Coord -> Tile)

mazes :: List Maze
mazes =
  Entry (Maze (C 1 1)       maze9) $
  Entry (Maze (C 0 0)       maze8) $
  Entry (Maze (C (-3) 3)    maze7) $
  Entry (Maze (C (-2) 4)    maze6) $
  Entry (Maze (C 0 1)       maze5) $
  Entry (Maze (C 1 (-3))    maze4) $
  Entry (Maze (C (-4) 3)    maze3) $
  Entry (Maze (C 0 1)       maze1) $
  Empty

extraMazes :: List Maze
extraMazes =
  Entry (Maze (C 1 (-3))    maze4') $
  Entry (Maze (C 1 (-3))    maze4'') $
  Entry (Maze (C 1 1)       maze9') $
  mazes

maze1 :: Coord -> Tile
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

maze3 :: Coord -> Tile
maze3 (C (-5) (-5)) = Wall
maze3 (C (-5) (-4)) = Wall
maze3 (C (-5) (-3)) = Wall
maze3 (C (-5) (-2)) = Wall
maze3 (C (-5) (-1)) = Wall
maze3 (C (-5)   0 ) = Wall
maze3 (C (-5)   1 ) = Wall
maze3 (C (-5)   2 ) = Wall
maze3 (C (-5)   3 ) = Wall
maze3 (C (-5)   4 ) = Wall

maze3 (C (-4) (-5)) = Wall
maze3 (C (-4) (-4)) = Ground
maze3 (C (-4) (-3)) = Ground
maze3 (C (-4) (-2)) = Ground
maze3 (C (-4) (-1)) = Ground
maze3 (C (-4)   0 ) = Ground
maze3 (C (-4)   1 ) = Ground
maze3 (C (-4)   2 ) = Ground
maze3 (C (-4)   3 ) = Ground
maze3 (C (-4)   4 ) = Wall

maze3 (C (-3) (-5)) = Wall
maze3 (C (-3) (-4)) = Ground
maze3 (C (-3) (-3)) = Wall
maze3 (C (-3) (-2)) = Wall
maze3 (C (-3) (-1)) = Wall
maze3 (C (-3)   0 ) = Wall
maze3 (C (-3)   1 ) = Ground
maze3 (C (-3)   2 ) = Wall
maze3 (C (-3)   3 ) = Ground
maze3 (C (-3)   4 ) = Wall
maze3 (C (-3)   5 ) = Wall

maze3 (C (-2) (-5)) = Wall
maze3 (C (-2) (-4)) = Box
maze3 (C (-2) (-3)) = Ground
maze3 (C (-2) (-2)) = Ground
maze3 (C (-2) (-1)) = Ground
maze3 (C (-2)   0 ) = Wall
maze3 (C (-2)   1 ) = Ground
maze3 (C (-2)   2 ) = Box
maze3 (C (-2)   3 ) = Box
maze3 (C (-2)   4 ) = Ground
maze3 (C (-2)   5 ) = Wall

maze3 (C (-1) (-6)) = Wall
maze3 (C (-1) (-5)) = Wall
maze3 (C (-1) (-4)) = Ground
maze3 (C (-1) (-3)) = Ground
maze3 (C (-1) (-2)) = Ground
maze3 (C (-1) (-1)) = Ground
maze3 (C (-1)   0 ) = Wall
maze3 (C (-1)   1 ) = Ground
maze3 (C (-1)   2 ) = Ground
maze3 (C (-1)   3 ) = Box
maze3 (C (-1)   4 ) = Ground
maze3 (C (-1)   5 ) = Wall
maze3 (C (-1)   6 ) = Wall

maze3 (C   0  (-6)) = Wall
maze3 (C   0  (-5)) = Ground
maze3 (C   0  (-4)) = Ground
maze3 (C   0  (-3)) = Ground
maze3 (C   0  (-2)) = Ground
maze3 (C   0  (-1)) = Ground
maze3 (C   0    0 ) = Wall
maze3 (C   0    1 ) = Wall
maze3 (C   0    2 ) = Wall
maze3 (C   0    3 ) = Wall
maze3 (C   0    4 ) = Ground
maze3 (C   0    5 ) = Ground
maze3 (C   0    6 ) = Wall

maze3 (C   1  (-6)) = Wall
maze3 (C   1  (-5)) = Ground
maze3 (C   1  (-4)) = Ground
maze3 (C   1  (-3)) = Ground
maze3 (C   1  (-2)) = Ground
maze3 (C   1  (-1)) = Ground
maze3 (C   1    0 ) = Wall
maze3 (C   1    1 ) = Storage
maze3 (C   1    2 ) = Storage
maze3 (C   1    3 ) = Storage
maze3 (C   1    4 ) = Ground
maze3 (C   1    5 ) = Ground
maze3 (C   1    6 ) = Wall

maze3 (C   2  (-6)) = Wall
maze3 (C   2  (-5)) = Wall
maze3 (C   2  (-4)) = Ground
maze3 (C   2  (-3)) = Ground
maze3 (C   2  (-2)) = Ground
maze3 (C   2  (-1)) = Ground
maze3 (C   2    0 ) = Wall
maze3 (C   2    1 ) = Wall
maze3 (C   2    2 ) = Wall
maze3 (C   2    3 ) = Wall
maze3 (C   2    4 ) = Wall
maze3 (C   2    5 ) = Wall
maze3 (C   2    6 ) = Wall

maze3 (C   3  (-5)) = Wall
maze3 (C   3  (-4)) = Ground
maze3 (C   3  (-3)) = Ground
maze3 (C   3  (-2)) = Storage
maze3 (C   3  (-1)) = Ground
maze3 (C   3    0 ) = Wall

maze3 (C   4  (-5)) = Wall
maze3 (C   4  (-4)) = Wall
maze3 (C   4  (-3)) = Wall
maze3 (C   4  (-2)) = Wall
maze3 (C   4  (-1)) = Wall
maze3 (C   4    0 ) = Wall

maze3 _ = Blank

maze4 :: Coord -> Tile
maze4 (C x y)
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x ==  2 && y <   0           = Wall
  | x >= -1 && y ==  1 && x <= 2 = Wall
  | x == -3 && y ==  1           = Wall
  | x ==  0 && y ==  3           = Wall
  | x ==  0 && y ==  0           = Wall
  | x ==  3 && y == -3           = Storage
  | x ==  1 && y ==  2           = Storage
  | x == -3 && y ==  2           = Storage
  | x ==  1 && y == -1           = Storage
  | x == -2 && y ==  1           = Box
  | x ==  2 && y ==  2           = Box
  | x <=  1 && y == -2 && x >= 0 = Box
  | otherwise                    = Ground

maze5 :: Coord -> Tile
maze5 (C x y)
  | abs x >  4 || abs y >  4           = Blank
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground

maze6 :: Coord -> Tile
maze6 (C x y)
  | abs x > 3  || abs y > 5                 = Blank
  | abs x == 3 || (abs y == 5 && abs x < 4) = Wall
  | x == 0 && abs y < 4                     = Storage
  | x == -1 && (y == 0 || abs y == 2)       = Box
  | x == 1 && (abs y == 1 || abs y == 3)    = Box
  | x == (-2) &&  y == 1                    = Wall
  | otherwise                               = Ground

maze7 :: Coord -> Tile
maze7 (C x y)
  | abs x > 4  || abs y > 4   = Blank
  | abs x == 4 || abs y == 4  = Wall
  | not (x == 2)  && y == 2   = Wall
  | not (x == -2)  && y == -1 = Wall
  | x ==  3 && y == -3        = Storage
  | x == 2 && y == 2          = Box
  | otherwise                 = Ground

maze8 :: Coord -> Tile
maze8 (C x y)
  | abs x > 10 || abs y > 10    = Blank
  | x == 0 && y == 0            = Ground
  | abs x == 9 && abs y == 9    = Wall
  | abs x == 10 || abs y == 10  = Wall
  | x == y                      = Storage
  | abs x == abs y              = Box
  | x < 0 && x > (-9) && y == 0 = Box
  | x > 0 && x < 9 && y == 0    = Storage
  | otherwise                   = Ground

maze9 :: Coord -> Tile
maze9 (C x y)
  | abs x > 4  || abs y > 4                  = Blank
  | abs x == 4 || abs y == 4 || x == -3      = Wall
  | x == -2 && (y == 3 || y == 0)            = Wall
  | x == -1 &&  y == -1                      = Wall
  | x == -0 &&  y == 1                       = Wall
  | x ==  3 &&  y == 0                       = Wall
  | x <   0 && (y == 2 || y == -3)           = Storage
  | x == -1 &&  y == 1                       = Storage
  | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
  | x ==  1 &&  y == -2                      = Box
  | x ==  2 &&  y == -3                      = Box
  | otherwise                                = Ground

maze4'' :: Coord -> Tile
maze4'' (C 1 (-3)) = Box
maze4'' c = maze4 c

maze4' :: Coord -> Tile
maze4' (C 0 1) = Blank
maze4' c = maze4 c

maze9' :: Coord -> Tile
maze9' (C 3 0) = Box
maze9' (C 4 0) = Box
maze9'  c      = maze9 c
