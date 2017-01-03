module CIS194.Week8 where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char
import Data.List (uncons)
import Data.Maybe

import System.Environment
import System.IO
import System.Exit

-- Exercise 1

data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

instance Functor (ComplicatedA a) where
  fmap f (Con1 x y) = Con1 x (f y)
  fmap f (Con2 xs) = Con2 (map (fmap (f .)) xs)

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

instance Functor g => Functor (ComplicatedB f g a) where
  fmap _ (Con3 x) = Con3 x
  fmap f (Con4 x) = Con4 (fmap f x)
  fmap f (Con5 x) = Con5 (fmap (fmap (map f)) x)

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
func3' xs = func1' xs <* xs

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
func5' xs ys = (+) <$> ((+1) <$> xs) <*> ((+1) <$> ys)

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' xs = fmap (\x -> if x > 0 then (x, 0) else (0, x)) xs

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
func8' xs x = (+ x) <$> xs

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

-- Nope, won't work because subsequent computation depends on result of previous
-- one.
func9' :: Functor f => f Integer -> f Integer -> f Integer -> f Integer
func9' = error "No possible implementation."

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' = fmap (\x -> (x*x) + 10)

-- Exercise 3

data Parser a = P (String -> Maybe (a, String))

runParser :: Parser t -> String -> Maybe (t, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = runParser p s >>= \(r, s') -> r <$ guard (null s')

noParser :: Parser a
noParser = P $ \_ -> Nothing

pureParser :: a -> Parser a
pureParser x = P $ \s -> Just (x, s)

instance Functor Parser where
  fmap f p = P $ fmap (\(r, s) -> (f r, s)) . runParser p

instance Applicative Parser where
    pure = pureParser
    P fp <*> P fx = P $ \s -> do
      (rp, sp) <- fp s
      (rx, sx) <- fx sp
      return (rp rx, sx)

instance Monad Parser where
    return = pureParser
    P fa >>= k = P $ \s -> do
      (ra, sa) <- fa s
      runParser (k ra) sa

anyChar :: Parser Char
anyChar = P uncons

char :: Char -> Parser ()
char c = (anyChar >>= \d -> if c == d then return () else noParser) `orElse` noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = (anyChar >>= \d -> if c /= d then return d else noParser) `orElse` noParser

orElse :: Parser a -> Parser a -> Parser a
orElse (P p) (P q) = P $ \s -> p s <|> q s

many :: Parser a -> Parser [a]
many p = go id
  where
    go xs = (p >>= \x -> go (xs . (x:))) `orElse` return (xs [])

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p sep = (flip ($) <$> p) `orElse` return (const $ return []) >>= go
  where
    go k = k $ \x -> do
      k' <- (id <$ sep) `orElse` return (const $ return [x])
      k' $ p >>= \y -> go (fmap (x:) . ($ y))

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

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do
  (anyChar >>= \c -> if p c then return c else noParser) `orElse` noParser

parseINI :: Parser INIFile
parseINI = many section
  where
    section      = (,) <$> title <*> (concat <$> many decl)
    title        = around '[' ']' ident <* nl
    decl         = emptyLine `orElse` commentLine `orElse` (pure <$> itemLine)
    itemLine     = (,) <$> ident <*> (s *> eq *> s *> restOfLine)
    emptyLine    = [] <$ nl
    commentLine  = [] <$ char '#' <* restOfLine
    ident        = (:) <$> identChar <*> many identChar
    s            = many (char ' ')
    eq           = char '='
    nl           = char '\n'
    identChar    = satisfies isAlphaNum
    restOfLine   = many (anyCharBut '\n') <* nl
    around l r m = char l *> m <* char r

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
