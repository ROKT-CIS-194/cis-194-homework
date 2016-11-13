{-# LANGUAGE OverloadedStrings #-}

--module CIS194.Week3 where

import CodeWorld

-- Lists

data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

allList :: List Bool -> Bool
allList (Entry a as) = a && allList as
allList Empty = True

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

-- Coordinates


data Coord = C Integer Integer

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2) = (x1 == x2) && (y1 == y2)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo from to c
  | eqCoord from c = to
  | otherwise      = c

-- The maze

data Tile = Wall | Ground | Storage | Box | Blank deriving (Show, Eq)

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

isOnStorage :: Coord -> Bool
isOnStorage c = (maze c == Storage)

noBoxMaze :: Coord -> Tile
noBoxMaze c = case maze c of
  Box -> Ground
  x   -> x

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes (Entry c cs) c2
  | eqCoord c c2 = Box
  | otherwise    = mazeWithBoxes cs c2
mazeWithBoxes _ c = noBoxMaze c


-- The state

data State = State Coord Direction (List Coord)

initialBoxes :: List Coord
initialBoxes = foldr (Entry) Empty $ filter (\c -> maze c == Box) [(C x y) | x <- [-10..10], y <- [-10..10]]

initialState :: State
initialState = State (C 0 1) R initialBoxes

isWon :: State -> Bool
isWon (State _ _ boxes) = allList (mapList isOnStorage boxes)


-- Event handling

moveIfAllowed :: State -> Direction -> State
moveIfAllowed (State from _ boxes) d
  | (at to == Box) && (allowed $ at next) = (State to d $ moveBoxes)
  | allowed (at to) = (State to d boxes)
  | otherwise = (State from d boxes)
  where at = mazeWithBoxes boxes
        to = adjacentCoord d from
        next = adjacentCoord d to
        moveBoxes = mapList (moveFromTo to next) boxes
        allowed t = (t == Ground || t == Storage)

handleEvent :: Event -> State -> State
handleEvent _ s
    | isWon s = s
handleEvent (KeyPress k) s
  | k == "Up"    = (moveIfAllowed s U)
  | k == "Down"  = (moveIfAllowed s D)
  | k == "Left"  = (moveIfAllowed s L)
  | k == "Right" = (moveIfAllowed s R)
handleEvent _ s = s

-- Drawing

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

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
draw s@(State c d boxes)
 | (isWon s) = (text "You won!")
 | otherwise = atCoord c (player d)
             & pictureOfBoxes boxes
             & pictureOfMaze

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
--main = drawingOf (pictureOfBoxes initialBoxes)
--main = drawingOf pictureOfMaze
main = runInteraction sokoban
