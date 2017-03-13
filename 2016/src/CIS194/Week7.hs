module CIS194.Week7 where

import Control.Monad (replicateM)
import System.Random

-- Exercise 1

fib :: Integer -> Integer
fib = undefined

fibs1 :: [Integer]
fibs1 = undefined

fibs2 :: [Integer]
fibs2 = undefined

-- Exercise 2

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList = undefined

instance Show a => Show (Stream a) where
    show = undefined

streamRepeat :: a -> Stream a
streamRepeat = undefined

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = undefined

streamIterate :: (a -> a) -> a -> Stream a
streamIterate = undefined

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave = undefined

nats :: Stream Integer
nats = undefined

ruler :: Stream Integer
ruler = undefined

-- Exercise 3

data Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = undefined

pureSupply :: a -> Supply s a
pureSupply = undefined

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply = undefined

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 = undefined

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply = undefined

runSupply :: Stream s -> Supply s a -> a
runSupply = undefined

instance Functor (Supply s) where
    fmap = mapSupply

instance Applicative (Supply s) where
    pure = pureSupply
    (<*>) = mapSupply2 id

instance Monad (Supply s) where
    return = pureSupply
    (>>=) = bindSupply

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

labelTree :: Tree a -> Tree Integer
labelTree = undefined

-- Non-Exercise

type Rand a = Supply Integer a

randomDice :: RandomGen g => g -> Stream Integer
randomDice gen =
    let (roll, gen') = randomR (1,6) gen
    in Cons roll (randomDice gen')

runRand :: Rand a -> IO a
runRand r = do
    stdGen <- getStdGen
    let diceRolls = randomDice stdGen
    return $ runSupply diceRolls r

averageOfTwo :: Rand Double
averageOfTwo = do
    d1 <- get
    d2 <- get
    return (fromIntegral (d1 + d2) / 2)

bestOutOfTwo :: Rand Double
bestOutOfTwo = do
    d1 <- get
    d2 <- get
    return $ fromIntegral $ if (d1 > d2) then d1 else d2

-- Look, ma, I'm recursive!
sumUntilOne :: Rand Double
sumUntilOne = do
    d <- get
    if (d == 1) then return 0
                else do s <- sumUntilOne
                        return (s + fromIntegral d)

sample :: Int -> Rand Double -> Rand (Double, Double)
sample n what = do
    samples <- replicateM n what
    return (maximum samples, sum samples / fromIntegral n)

main :: IO ()
main = mapM_ go [ ("average of two", averageOfTwo)
                , ("bestOutOfTwo",   bestOutOfTwo)
                , ("sumUntilOne",    sumUntilOne)
                ]
  where
    n = 10000
    go (name, what) = do
        (max, avg) <- runRand (sample n what)
        putStrLn $ "Playing \"" ++ name ++ "\" " ++ show n ++ " times " ++
                   "yields a max of " ++ show max ++ " and an average of " ++
                   show avg ++ "."
