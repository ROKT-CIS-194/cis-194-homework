{-# LANGUAGE OverloadedStrings #-}

module CIS194.Week3 where

import CodeWorld

-- Lists

data List a = Empty | Entry a (List a) deriving (Eq, Show)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList p (Entry x xs) | p x       = Entry x $ filterList p xs
                          | otherwise = filterList p xs

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

elemList :: Eq a => a -> List a -> Bool
elemList _ Empty        = False
elemList x (Entry y ys) | x == y    = True
                        | otherwise = elemList x ys

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

-- Coordinates


data Coord = C Integer Integer deriving (Show, Eq)

data Direction = R | U | L | D deriving (Show, Eq)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

allCoords :: List Coord
allCoords = go Empty 10 10
              where go cs x y | x <= (-10) && y <= (-10) = (Entry (C x y) cs)
                              | x <= (-10)               = go (Entry (C x y) cs) 10 (y - 1)
                              | otherwise                = go (Entry (C x y) cs) (x - 1) y


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

noBoxMaze :: Coord -> Tile
noBoxMaze c = case m of Box -> Ground
                        _   -> m
                where m = maze c

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes bs c = if elemList c bs then Box else noBoxMaze c

-- The state

data State = State { playerPos :: Coord
                   , playerDir :: Direction
                   , boxes     :: (List Coord)
                   } deriving (Show)

initialBoxes :: List Coord
initialBoxes = filterList ((== Box) . maze) allCoords

initialState :: State
initialState = State { playerPos = case (filterList availableTile allCoords) of
                                     Entry c _ -> c
                                     Empty     -> C 1 2
                     , playerDir = R
                     , boxes = initialBoxes
                     }
                 where availableTile c = (notBox c) && (isGround c)
                       notBox c        = not $ elemList c initialBoxes
                       isGround c      = (noBoxMaze c) == Ground

-- Event handling

handleEvent :: Event -> State -> State
handleEvent e s@State {playerDir = dir, playerPos = pos, boxes = bs}
  | isWon s   = s
  | otherwise = State { playerDir = newDir, playerPos = newPos', boxes = newBs' }
  where
    (newDir, moved) = case e of
      KeyPress "Up"    -> (U, True)
      KeyPress "Down"  -> (D, True)
      KeyPress "Left"  -> (L, True)
      KeyPress "Right" -> (R, True)
      _                -> (dir, False)
    newPos = if moved then adjacentCoord newDir pos else pos
    newBs = mapList (\p -> if p == newPos then (adjacentCoord newDir p) else p) bs
    (newPos', newBs') = if isValidMaze newPos newBs then (newPos, newBs) else (pos, bs)

isValidMaze :: Coord -> (List Coord) -> Bool
isValidMaze position boxes = go position boxes
  where
    go pos bs
      | isBlankOrWall pos                      = False
      | (filterList isBlankOrWall bs) /= Empty = False
      | elemList pos bs                        = False
      | hasDupes bs                            = False
      | otherwise                              = True
    isBlankOrWall c = case maze c of
      Blank -> True
      Wall  -> True
      _     -> False


isStorage :: Coord -> Bool
isStorage c = maze c == Storage

isWon :: State -> Bool
isWon State {boxes = bs} = allList $ mapList isStorage bs


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
draw s@(State {playerPos=pos, playerDir=dir, boxes=bs})
  | isWon s   = scaled 3 3 (text "You won!")
  | otherwise = (atCoord pos $ player dir) & pictureOfBoxes bs & pictureOfMaze

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
main = runInteraction $ resetable $ withStartScreen sokoban
