module CIS194.Week9 where

import Data.Char
import Data.Maybe
import Data.List
import System.Environment
import System.IO
import System.Exit
import Prelude hiding (exp)

-- Exercise 1

parseBNF :: Descr f => f BNF
parseBNF = pBNF

-- identifier = letter, {letter | digit | '-'};
pIdentifier :: Descr f => f String
pIdentifier = nonTerminal "identifier" $
  (:) <$> letter <*> many (letter `orElse` digit `orElse` ('-' <$ char '-'))

-- quoted-char = non-quote-or-backslash | '\\', '\\' | '\\', '\'';
pQuotedChar :: Descr f => f Char
pQuotedChar = nonTerminal "quoted-char" $
  notQuoteOrBackslash `orElse` f '\\' `orElse` f '\''
  where f c = c <$ char '\\' <* char c

-- terminal = '\'', {quoted-char}, '\'', spaces;
pTerminal :: Descr f => f RHS
pTerminal = nonTerminal "terminal" $
  Terminal <$> (char '\'' *> many pQuotedChar <* char '\'' <* spaces)

-- non-terminal = identifier, spaces;
pNonTerminal :: Descr f => f RHS
pNonTerminal = nonTerminal "non-terminal" $
  NonTerminal <$> pIdentifier <* spaces

-- option = '[', spaces, rhs, spaces, ']', spaces;
pOption :: Descr f => f RHS
pOption = nonTerminal "option" $
  Optional <$> (char '[' *> spaces *> pRHS <* spaces <* char ']') <* spaces

-- repetition = '{', spaces, rhs, spaces, '}', spaces;
pRepetition :: Descr f => f RHS
pRepetition = nonTerminal "repetition" $
  Repetition <$> (char '{' *> spaces *> pRHS <* spaces  <* char '}') <* spaces

-- group = '(', spaces, rhs, spaces, ')', spaces;
pGroup :: Descr f => f RHS
pGroup = nonTerminal "group" $
  char '(' *> spaces *> pRHS <* spaces <* char ')' <* spaces

-- atom = terminal | non-terminal | option | repetition | group;
pAtom :: Descr f => f RHS
pAtom = nonTerminal "atom" $
  foldr1 orElse [pTerminal, pNonTerminal, pOption, pRepetition, pGroup]

-- sequence = atom, {spaces, ',', spaces, atom}, spaces;
pSequence :: Descr f => f RHS
pSequence = nonTerminal "sequence" $
  mkSequences <$> pAtom <*> many (spaces *> char ',' *> spaces *> pAtom) <* spaces

-- choice = sequence, {spaces, '|', spaces, sequence}, spaces;
pChoice :: Descr f => f RHS
pChoice = nonTerminal "choice" $
  mkChoices <$> pSequence <*> many (spaces *> char '|' *> spaces *> pSequence) <* spaces

-- rhs = choice;
pRHS :: Descr f => f RHS
pRHS = nonTerminal "rhs" $ pChoice

-- production = identifier, spaces, '=', spaces, rhs, ';', spaces;
pProduction :: Descr f => f Production
pProduction = nonTerminal "production" $
  (,) <$> pIdentifier <*> (spaces *> char '=' *> spaces *> pRHS <* char ';' <* spaces)

-- bnf = production, {production};
pBNF :: Descr f => f BNF
pBNF = nonTerminal "bnf" $
  (:) <$> pProduction <*> many pProduction

-- Example: Simple expressions

data Expr = Plus Expr Expr | Mult Expr Expr | Const Integer
  deriving Show

mkPlus :: Expr -> [Expr] -> Expr
mkPlus = foldl Plus

mkMult :: Expr -> [Expr] -> Expr
mkMult = foldl Mult

parseExpr :: Descr f => f Expr
parseExpr = nonTerminal "expr" $ ePlus

ePlus :: Descr f => f Expr
ePlus = nonTerminal "plus" $
  mkPlus <$> eMult
         <*> many (spaces *>  char '+' *>  spaces *> eMult)
         <*  spaces

eMult :: Descr f => f Expr
eMult = nonTerminal "mult" $
  mkPlus <$> eAtom
         <*> many (spaces *>  char '*' *>  spaces *> eAtom)
         <*  spaces

eAtom :: Descr f => f Expr
eAtom = nonTerminal "atom" $
  aConst `orElse` eParens parseExpr

aConst :: Descr f => f Expr
aConst = nonTerminal "const" $ Const . read <$> many1 digit

eParens :: Descr f => f a -> f a
eParens inner =
  id <$  char '('
     <*  spaces
     <*> inner
     <*  spaces
     <*  char ')'
     <*  spaces

-- Example: EBNF in Haskell

type BNF = [Production]

type Production = (String, RHS)

data RHS
  = Terminal String
  | NonTerminal String
  | Choice RHS RHS
  | Sequence RHS RHS
  | Optional RHS
  | Repetition RHS
    deriving (Show, Eq)

mkChoices :: RHS -> [RHS] -> RHS
mkChoices = foldl Choice

mkSequences :: RHS -> [RHS] -> RHS
mkSequences = foldl Sequence

ppRHS :: RHS -> String
ppRHS = go (0 :: Int)
  where
    go _ (Terminal s)     = surround "'" "'" $ concatMap quote s
    go _ (NonTerminal s)  = s
    go a (Choice x1 x2)   = p a 1 $ go 1 x1 ++ " | " ++ go 1 x2
    go a (Sequence x1 x2) = p a 2 $ go 2 x1 ++ ", "  ++ go 2 x2
    go _ (Optional x)     = surround "[" "]" $ go 0 x
    go _ (Repetition x)   = surround "{" "}" $ go 0 x

    surround c1 c2 x = c1 ++ x ++ c2

    p a n | a > n     = surround "(" ")"
          | otherwise = id

    quote '\'' = "\\'"
    quote '\\' = "\\\\"
    quote c    = [c]

ppBNF :: BNF -> String
ppBNF = unlines . map (\(i,rhs) -> i ++ " = " ++ ppRHS rhs ++ ";")

-- The parser

newtype Parser a = P (String -> Maybe (a, String))

runParser :: Parser t -> String -> Maybe (t, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p input = case runParser p input of
  Just (result, "") -> Just result
  _ -> Nothing -- handles both no result and leftover input

noParserP :: Parser a
noParserP = P (\_ -> Nothing)

pureParserP :: a -> Parser a
pureParserP x = P (\input -> Just (x,input))

instance Functor Parser where
  fmap f p = P p'
    where
      p' input = case runParser p input of
        Just (result, rest) -> Just (f result, rest)
        Nothing -> Nothing

instance Applicative Parser where
  pure = pureParserP
  p1 <*> p2 = P $ \input -> do
    (f, rest1) <- runParser p1 input
    (x, rest2) <- runParser p2 rest1
    return (f x, rest2)

instance Monad Parser where
  return = pure
  p1 >>= k = P $ \input -> do
    (x, rest1) <- runParser p1 input
    runParser (k x) rest1

anyCharP :: Parser Char
anyCharP = P $ \input -> case input of
  (c:rest) -> Just (c, rest)
  [] -> Nothing

charP :: Char -> Parser ()
charP c = do
  c' <- anyCharP
  if c == c' then return () else noParserP

anyCharButP :: Char -> Parser Char
anyCharButP c = do
  c' <- anyCharP
  if c /= c' then return c' else noParserP

letterOrDigitP :: Parser Char
letterOrDigitP = do
  c <- anyCharP
  if isAlphaNum c then return c else noParserP

orElseP :: Parser a -> Parser a -> Parser a
orElseP p1 p2 = P $ \input -> case runParser p1 input of
  Just r -> Just r
  Nothing -> runParser p2 input

manyP :: Parser a -> Parser [a]
manyP p = ((:) <$> p <*> manyP p) `orElseP` return []

many1P :: Parser a -> Parser [a]
many1P p = pure (:) <*> p <*> manyP p

sepByP :: Parser a -> Parser () -> Parser [a]
sepByP p1 p2 = ((:) <$> p1 <*> (manyP (p2 >> p1))) `orElseP` return []

nonTerminalP :: String -> Parser a -> Parser a
nonTerminalP _ p = let r = const p r in r

-- A grammar-producing type constructor

newtype Grammar a = G ([String] -> (BNF, RHS))

runGrammar :: String -> Grammar a -> BNF
runGrammar str (G f) = case f [] of
  (prods, NonTerminal nt) | str == nt -> prods
  (prods, rhs) -> prods ++ [(str, rhs)]

ppGrammar :: String -> Grammar a -> String
ppGrammar str g = ppBNF $ runGrammar str g

charG :: Char -> Grammar ()
charG c = G $ \_ -> ([], Terminal [c])

anyCharG :: Grammar Char
anyCharG = G $ \_ -> ([], NonTerminal "char")

manyG :: Grammar a -> Grammar [a]
manyG (G f) = G $ fmap Repetition . f

mergeProds :: [Production] -> [Production] -> [Production]
mergeProds prods1 prods2 = nub $ prods1 ++ prods2

orElseG :: Grammar a -> Grammar a -> Grammar a
orElseG (G f) (G g) = G $ \seen -> case (f seen, g seen) of
  ((prods1, rhs1), (prods2, rhs2)) ->
    (mergeProds prods1 prods2, Choice rhs1 rhs2)

instance Functor Grammar where
  fmap _ (G bnf) = G bnf

instance Applicative Grammar where
  pure _ = G $ \_ -> ([], Terminal "")
  G f <*> G g = G $ \seen -> case (f seen, g seen) of
    ((prods1, Terminal ""), (prods2, rhs2)) ->
      (mergeProds prods1 prods2, rhs2)
    ((prods1, rhs1), (prods2, Terminal "")) ->
      (mergeProds prods1 prods2, rhs1)
    ((prods1, rhs1), (prods2, rhs2)) ->
      (mergeProds prods1 prods2, Sequence rhs1 rhs2)

many1G :: Grammar a -> Grammar [a]
many1G p = pure (:) <*> p <*> manyG p

sepByG :: Grammar a -> Grammar () -> Grammar [a]
sepByG p1 p2 = ((:) <$> p1 <*> (manyG (p2 *> p1))) `orElseG` pure []

primitiveG :: String -> Grammar a
primitiveG s = G $ \_ -> ([], NonTerminal s)

newlineG :: Grammar ()
newlineG = primitiveG "newline"

nonTerminalG :: String -> Grammar a -> Grammar a
nonTerminalG name (G g) = G $ \seen ->
    if name `elem` seen
    then ([], NonTerminal name)
    else let (prods, rhs) = g (name : seen)
         in (prods ++ [(name, rhs)], NonTerminal name)



-- The generic approach

class Applicative f => Descr f where
  char :: Char -> f ()
  many :: f a -> f [a]
  orElse :: f a -> f a -> f a
  primitive :: String -> Parser a -> f a
  nonTerminal :: String -> f a -> f a

instance Descr Parser where
  char = charP
  many = manyP
  orElse = orElseP
  primitive _ p = p
  nonTerminal = nonTerminalP

instance Descr Grammar where
  char = charG
  many = manyG
  orElse = orElseG
  primitive s _ = primitiveG s
  nonTerminal = nonTerminalG

many1 :: Descr f => f a -> f [a]
many1 p = pure (:) <*> p <*> many p

sepBy :: Descr f => f a -> f () -> f [a]
sepBy p1 p2 = ((:) <$> p1 <*> (many (p2 *> p1))) `orElse` pure []

newline :: Descr f => f ()
newline = primitive "newline" (charP '\n')

anyChar :: Descr f => f Char
anyChar = primitive "char" anyCharP

letter :: Descr f => f Char
letter = primitive "letter" $ do
  c <- anyCharP
  if isLetter c then return c else noParserP

digit :: Descr f => f Char
digit = primitive "digit" $ do
  c <- anyCharP
  if isDigit c then return c else noParserP

notQuoteOrBackslash :: Descr f => f Char
notQuoteOrBackslash = primitive "non-quote-or-backslash" $ do
  c <- anyCharP
  if c `notElem` "\\'" then return c else noParserP

spaces :: Descr f => f ()
spaces = nonTerminal "spaces" $
  () <$ many (char ' ' `orElse` newline)

-- The main function

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStr $ ppGrammar "bnf" parseBNF
    [fileName] -> do
      input <- readFile fileName
      case parse parseBNF input of
        Just i -> putStr $ ppBNF i
        Nothing -> hPutStrLn stderr "Failed to parse INI file." >> exitFailure
    _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
