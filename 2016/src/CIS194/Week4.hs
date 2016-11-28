{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week4 where

import CodeWorld

-- Coordinates

data Coord =
  C Integer Integer
  deriving (Show, Eq)

data Direction =
  R | U | L | D
  deriving (Show, Eq)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

gridSize :: Integer
gridSize = 10

allCoords :: List Coord
allCoords = foldr Entry Empty [(C x y) | x <- [(-gridSize)..gridSize], y <- [(-gridSize)..gridSize]]

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
elemList x xs = any (== x) xs

appendList :: List a -> List a -> List a
-- Why doesn't this work?
-- appendList = (foldr Entry) . flip
appendList xs ys = foldr Entry ys xs

listLength :: List a -> Integer
listLength = foldl (\x _ -> x + 1) 0

filterList :: (a -> Bool) -> List a -> List a
filterList f = foldr (\x xs -> if f x then Entry x xs else xs) Empty

nth :: List a -> Integer -> a
nth (Entry x _)  0 = x
nth (Entry _ xs) n = nth xs (n-1)
nth Empty        _ = error "list too short"

hasDupes :: Eq a => List a -> Bool
hasDupes Empty        = False
hasDupes (Entry x xs) = go x xs
  where go _ Empty        = False
        go y (Entry z zs) | y == z        = True
                          | elemList y zs = True
                          | otherwise     = go z zs

allList :: List Bool -> Bool
allList Empty        = True
allList (Entry x xs) | not x     = False
                     | otherwise = allList xs


-- Exercise 2: Graph Search

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk =
  go Empty (Entry initial Empty)
  where
    go _ Empty = True
    go visited (Entry n ns)
      | not (isOk n) = False
      | otherwise    = let neighbours = filterList (not . (`elemList` (appendList visited ns))) $ adjacent n
                       in case neighbours of
                         Empty     -> go (Entry n visited) ns
                         Entry _ _ -> go (Entry n visited) (appendList ns neighbours)

-- Exercise 3: Check closedness of mazes

allDirections :: List Direction
allDirections = Entry U $ Entry D $ Entry L $ Entry R Empty

isClosed :: Maze -> Bool
isClosed (Maze start tile) =
  tile start `elem` [Ground, Storage] && isGraphClosed start adjacent isOk
  where
    adjacent pos       = filterList isWalkable $ mapList (\c -> adjacentCoord c pos) allDirections
    isInBounds (C x y) = (abs x) <= gridSize && (abs y) <= gridSize
    isNotWall          = (/= Wall) . tile
    isWalkable c       = isInBounds c && isNotWall c
    isOk               = (`elem` [Ground, Storage, Box]) . tile

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
exercise3 = drawingOf $ pictureOfBools $ mapList isClosed extraMazes

-- Exercise 4: Multi-Level Sokoban
-- Extend your game from last week (or the code from the lecture) to implement
-- multi-level sokoban.



exercise4 :: IO ()
exercise4 = runInteraction $ resetable $ withStartScreen sokoban

main :: IO ()
main = return ()


-- MAZES

data Tile =
  Wall | Ground | Storage | Box | Blank
  deriving (Eq, Show)

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


-- Sokoban implementation, mostly copied from last time

-- The state

data State = State { stateMaze           :: (Coord -> Tile)
                   , statePlayerPos      :: Coord
                   , statePlayerDir      :: Direction
                   , stateBoxes          :: (List Coord)
                   , stateRemainingMazes :: (List Maze)
                   , stateLevel          :: Integer
                   }

initialBoxes :: (Coord -> Tile) -> List Coord
initialBoxes maze = filterList ((== Box) . maze) allCoords

noBoxMaze :: (Coord -> Tile) -> Coord -> Tile
noBoxMaze maze c = let m = maze c
                   in case m of
                        Box -> Ground
                        _   -> m

loadLevel :: Integer -> List Maze -> State
loadLevel _ Empty = error "no more levels"
loadLevel l (Entry (Maze start maze) ms) =
  State { stateMaze = maze
        , statePlayerPos = start
        , statePlayerDir = R
        , stateBoxes = initialBoxes maze
        , stateRemainingMazes = ms
        , stateLevel = l + 1
        }

initialState :: List Maze -> State
initialState ms = loadLevel 0 ms

nextLevel :: State -> State
nextLevel State { stateLevel = l, stateRemainingMazes = ms@(Entry _ _) } = loadLevel l ms
nextLevel s = s

-- Event handling

handleEvent :: Event -> State -> State
handleEvent e s@State {stateMaze = maze, statePlayerDir = dir, statePlayerPos = pos, stateBoxes = bs}
  | isWon s   = if e == KeyPress " " then nextLevel s else s
  | otherwise = s { statePlayerDir = newDir, statePlayerPos = newPos', stateBoxes = newBs' }
  where
    (newDir, moved) = case e of
      KeyPress "Up"    -> (U, True)
      KeyPress "Down"  -> (D, True)
      KeyPress "Left"  -> (L, True)
      KeyPress "Right" -> (R, True)
      _                -> (dir, False)
    newPos = if moved then adjacentCoord newDir pos else pos
    newBs = mapList (\p -> if p == newPos then (adjacentCoord newDir p) else p) bs
    (newPos', newBs') = if isValidMaze maze newPos newBs then (newPos, newBs) else (pos, bs)

isValidMaze :: (Coord -> Tile) -> Coord -> (List Coord) -> Bool
isValidMaze m position boxes = go position boxes
  where
    go pos bs
      | isBlankOrWall pos                      = False
      | (filterList isBlankOrWall bs) /= Empty = False
      | elemList pos bs                        = False
      | hasDupes bs                            = False
      | otherwise                              = True
    isBlankOrWall c = case m c of
      Blank -> True
      Wall  -> True
      _     -> False


isStorage :: (Coord -> Tile) -> Coord -> Bool
isStorage m = (== Storage) . m

isWon :: State -> Bool
isWon State {stateMaze = maze, stateBoxes = bs} = allList $ mapList (isStorage maze) bs


-- Drawing

wall, ground, storage, box :: Picture
wall    = colored (grey 0.4) (solidRectangle 1 1)
ground  = colored yellow     (solidRectangle 1 1)
storage = colored white      (solidCircle 0.3) & ground
box     = colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: (Coord -> Tile) -> Picture
pictureOfMaze m = draw21times (\r -> draw21times (\c -> drawTileAt m (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: (Coord -> Tile) -> Coord -> Picture
drawTileAt m c = atCoord c (drawTile (noBoxMaze m c))


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)]
         & path [(0,0),(0.3,-0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)]
         & path [(0,0),(-0.3,0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & path [(0,0),(0.3,-0.05)]
         & path [(0,0),(-0.3,-0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

draw :: State -> Picture
draw s@(State { statePlayerPos = pos, statePlayerDir = dir, stateBoxes = bs, stateMaze = maze })
  | isWon s   = scaled 3 3 (text "You won!")
  | otherwise = (atCoord pos $ player dir) & pictureOfBoxes bs & pictureOfMaze maze

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction (initialState extraMazes) (\_ c -> c) handleEvent draw

-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle d)
  = interactionOf state0 step handle d

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle d)
  = Interaction state0 step handle' d
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle d)
  = Interaction state0' step' handle' d'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)

    d' StartScreen = startScreen
    d' (Running s) = d s
