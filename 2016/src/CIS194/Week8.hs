module CIS194.Week8 where

import Control.Applicative ((<|>))
import Control.Monad (liftM2)
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

instance Functor (ComplicatedA a) where
    fmap f (Con1 a b) = Con1 a (f b)
    fmap f (Con2 l) = Con2 (map (fmap (f .)) l)

instance Functor g => Functor (ComplicatedB f g a) where
    fmap f (Con3 x) = Con3 x
    fmap f (Con4 y) = Con4 (fmap f y)
    fmap f (Con5 z) = Con5 (fmap (fmap (map f)) z)

-- Exercise 2

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = fmap (f . f) xs

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a,a)
func1' xs = fmap (\x -> (x,x)) xs

func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a,a)
func2' xs = (,) <$> xs <*> xs

func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a,a)
func3' xs = (\x _ -> (x,x)) <$> xs <*> xs

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Monad f => f a -> f a -> f (a,a)
func4' xs ys = (,) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (\x y -> (x+1) + (y+1)) <$> xs <*> ys

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' xs = (\x -> if x > 0 then (x,0) else (0,x)) <$> xs

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
func8' xs x = (+x) <$> xs

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func9' :: Functor f => f Integer -> f Integer -> f Integer -> f Integer
func9' = undefined

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' xs = (\x -> (x*x) + 10) <$> xs

-- Exercise 3

data Parser a = P (String -> Maybe (a, String))

runParser :: Parser t -> String -> Maybe (t, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = case runParser p s of
    Just (result, "") -> Just result
    _ -> Nothing

noParser :: Parser a
noParser = P (\_ -> Nothing)

pureParser :: a -> Parser a
pureParser x = P (\s -> Just (x, s))

instance Functor Parser where
    fmap f p = P (fmap (\(x,s) -> (f x, s)) . runParser p)

instance Applicative Parser where
    pure = pureParser
    ff <*> fx = P (\input ->
                    do
                      (f, input')  <- runParser ff input
                      (x, input'') <- runParser fx input'
                      return (f x, input''))

instance Monad Parser where
    return = pureParser
    fx >>= k = P (\input ->
                   do
                     (x, input') <- runParser fx input
                     runParser (k x) input')

anyChar :: Parser Char
anyChar = P (\input -> case input of
                         (c:cs) -> Just (c, cs)
                         [] -> Nothing)

char :: Char -> Parser ()
char c = do
           c' <- anyChar
           if c == c' then return () else noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = do
                 c' <- anyChar
                 if c == c' then noParser else return c'

orElse :: Parser a -> Parser a -> Parser a
orElse (P p1) (P p2) = P (liftM2 (<|>) p1 p2)

many :: Parser a -> Parser [a]
many p = orElse (pure (:) <*> p <*> many p)
                (pure [])

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p sep = orElse (pure (:) <*> p <*> (many (sep *> p)))
                     (pure [])

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
parseINI = many1 section
  where section = do
                    char '['
                    title <- identifier
                    char ']'
                    newLine
                    body <- sectionBody
                    return (title, catMaybes body)
        identifier = many1 letterOrDigit
        sectionBody = many line
        line = declaration `orElse` emptyLine `orElse` commentLine
        declaration = do
                        i <- identifier
                        spaces
                        char '='
                        spaces
                        val <- many1 notNewLine
                        newLine
                        return $ Just (i, val)
        commentLine = do
                        char '#'
                        many notNewLine
                        newLine
                        return Nothing
        emptyLine = do
                      newLine
                      return Nothing
        --
        many1 p = (:) <$> p <*> many p
        letterOrDigit = do
                          c <- anyChar
                          if isAlphaNum c then return c else noParser
        newLine = char '\n'
        notNewLine = anyCharBut '\n'
        spaces = many (char ' ')

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
