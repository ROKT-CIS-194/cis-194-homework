{-# LANGUAGE TupleSections #-}
module CIS194.Week8 where

import           Data.Char
import           Data.Maybe

import           System.Environment
import           System.Exit
import           System.IO

-- Exercise 1

data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

instance Functor (ComplicatedA a) where
  fmap f (Con1 a b) = Con1 a $ f b
  fmap f (Con2 gs)  = Con2 $ map (fmap (f .)) gs

-- Exercise 2

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f = fmap (f . f)

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a,a)
func1' = fmap (\x -> (x, x))

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
func5' xs ys = (+) <$> fmap (+1) xs <*> fmap (+1) ys


func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' = fmap (\x -> if x > 0 then (x, 0) else (0, x))

func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

func7' :: Functor f => f Integer -> f (Integer,Integer)
func7' = func6'

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = fmap (+ x) xs

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func9' :: Functor f => f Integer -> f Integer -> f Integer -> f Integer
func9' _xs _ys _zs = undefined
-- This one's not possible. Without an Applicative instance for the Functor, you
-- can only produce `f Integer` from `xs`, so the only thing you could do to
-- `ys` and `zs` is choose between them for the result of `func9'` -- you can't
-- produce a function that could combine them in any way.

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' = fmap ((+ 10) . (^ (2 :: Integer)))

-- Exercise 3

data Parser a = P (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p = fmap fst . runParser p

noParser :: Parser a
noParser = P $ const Nothing

pureParser :: a -> Parser a
pureParser x = P $ \s -> Just (x, s)

instance Functor Parser where
    fmap f p = P $ \s -> case runParser p s of
      Just (x, s') -> Just (f x, s')
      Nothing      -> Nothing

instance Applicative Parser where
    pure = pureParser
    fp <*> fx = P $ \s -> case runParser fp s of
      Just (fo, s') -> case runParser fx s' of
        Just (xo, s'') -> Just (fo xo, s'')
        Nothing        -> Nothing
      Nothing       -> Nothing

instance Monad Parser where
    return = pureParser
    fa >>= k = P $ \s -> case runParser fa s of
      Just (ao, s') -> runParser (k ao) s'
      Nothing       -> Nothing

anyChar :: Parser Char
anyChar = P $ \s -> case s of
  ""     -> Nothing
  (c:cs) -> Just (c, cs)

char :: Char -> Parser ()
char c = anyChar >>= \c' -> if c' == c then pureParser () else noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = anyChar >>= \c' -> if c' == c then noParser else pureParser c'

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = P $ \s -> case runParser p1 s of
  Just x  -> Just x
  Nothing -> runParser p2 s

many :: Parser a -> Parser [a]
many p = (p >>= \a -> (a:) <$> many p) `orElse` pureParser []

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = (p1 >>= \a -> (a:) <$> many (p2 *> p1)) `orElse` pureParser []

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

type Identifier = String
type Declaration = (Identifier, String)
type Section = (Identifier, [Declaration])
type INIFile = [Section]

parseINI :: Parser INIFile
parseINI = many parseSection

parseSection :: Parser Section
parseSection = (,) <$> parseSectionHeader <*> (catMaybes <$> many parseSectionContentLine)

parseSectionHeader :: Parser Identifier
parseSectionHeader = char '[' *> many (anyCharBut ']') <* char ']' <* char '\n'
-- FIXME: accepts "[]\n"

parseSectionContentLine :: Parser (Maybe Declaration)
parseSectionContentLine = parseBlankLine `orElse` parseCommentLine `orElse` parseDeclaration

parseBlankLine :: Parser (Maybe a)
parseBlankLine = const Nothing <$> char '\n'

parseCommentLine :: Parser (Maybe a)
parseCommentLine = fmap (const Nothing) $ char '#' <* many (anyCharBut '\n') <* char '\n'

parseDeclaration :: Parser (Maybe Declaration)
parseDeclaration = fmap Just $ (,)
  <$> ((:) <$> anyCharBut '[' <*> many (anyCharBut ' ')) <* many (char ' ') <* char '=' <* many (char ' ')
  <*> many (anyCharBut '\n') <* char '\n'

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
