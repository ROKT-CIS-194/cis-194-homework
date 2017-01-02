module CIS194.Week8 where

import Data.Char
import Data.Maybe

import System.Environment
import System.IO
import System.Exit

-- Exercise 1

data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

-- Exercise 2

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' = undefined

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a,a)
func1' = undefined

func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a,a)
func2' = undefined

func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a,a)
func3' = undefined

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Monad f => f a -> f a -> f (a,a)
func4' = undefined

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' = undefined


func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' = undefined

func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

func7' :: Functor f => f Integer -> f (Integer,Integer)
func7' = undefined

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' = undefined

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func9' :: Functor f => f Integer -> f Integer -> f Integer -> f Integer
func9' = undefined

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' = undefined

-- Exercise 3

data Parser a = P ()

runParser :: Parser t -> String -> Maybe (t, String)
runParser (P p) = undefined

parse :: Parser a -> String -> Maybe a
parse = undefined

noParser :: Parser a
noParser = undefined

pureParser :: a -> Parser a
pureParser = undefined

instance Functor Parser where
    fmap = undefined

instance Applicative Parser where
    pure = pureParser
    fp <*> fx = undefined

instance Monad Parser where
    return = pureParser
    fa >>= k = undefined

anyChar :: Parser Char
anyChar = undefined

char :: Char -> Parser ()
char = undefined

anyCharBut :: Char -> Parser Char
anyCharBut = undefined

orElse :: Parser a -> Parser a -> Parser a
orElse = undefined

many :: Parser a -> Parser [a]
many = undefined

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy = undefined

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content

-- Exercise 4

type Identifer = String
type Declaration = (Identifer, String)
type Section = (Identifer, [Declaration])
type INIFile = [Section]

parseINI :: Parser INIFile
parseINI = undefined

main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        [] -> getContents
        [fileName] -> readFile fileName
        _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
    case parse parseINI input of
        Just i -> print i
        Nothing -> do
            hPutStrLn stderr "Failed to parse INI file."
            exitFailure

example :: String
example =
  unlines ["[requests]"
          ,"desiredFood = cookies"
          ,"desiredQuantity = 20"
          ,""
          ,"[supply]"
          ,"flour = 20 ounzes"
          ,""
          ,"sugar = none!"
          ,"[conclusion]"
          ,"# none!"]
