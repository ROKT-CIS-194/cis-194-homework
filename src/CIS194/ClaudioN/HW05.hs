{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module CIS194.ClaudioN.HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import Data.Bits (xor)
import Data.List (sortBy)
import Data.Ord (comparing)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import CIS194.ClaudioN.HW05.Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret orig modified = do
  a <- BS.readFile orig
  b <- BS.readFile modified
  return $ BS.pack $ filter (/= 0) $ BS.zipWith xor a b

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key file = do
  encrypted <- BS.readFile $ file ++ ".enc"
  let decrypted = BS.pack $ BS.zipWith xor encrypted $ BS.cycle key
  BS.writeFile file decrypted

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = decode <$> BS.readFile file

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vict trans = do
  victims <- parseFile vict :: IO (Maybe [TId])
  transactions <- parseFile trans
--  return $ case victims of
--    Nothing -> Nothing
--    (Just vs) -> filter (\x -> elem (tid x) vs) <$> transactions
  return $ (\vs ts -> filter (\x -> elem (tid x) vs) ts) <$> victims <*> transactions

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr adjustFlow Map.empty
  where adjustFlow (Transaction from to amount _) = add from (-amount) . add to amount
        add = Map.insertWith (+)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.foldrWithKey findCrim ("", 0) -- FIXME: no doubt there's a more idiomatic way to kick this off...
  where findCrim name amount c@(_, maxAmount)
          | amount > maxAmount = (name, amount)
          | otherwise = c

-- Exercise 7 -----------------------------------------

newTrans :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
newTrans ((_, 0):payers) payees tids = newTrans payers payees tids
newTrans payers ((_, 0):payees) tids = newTrans payers payees tids
newTrans ((payer, payerAmount):payers) ((payee, payeeAmount):payees) (tid:tids) = tran : restTrans
  where amt = min payerAmount (-payeeAmount)
        tran = Transaction payer payee amt tid
        newpayer = (payer, payerAmount - amt)
        newpayee = (payee, payeeAmount + amt)
        restTrans = newTrans (newpayer:payers) (newpayee:payees) tids
newTrans [] [] _ = []
newTrans _ _ _ = error "Imbalanced lists of payers and payees"

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m = newTrans (sortItems flip payers) (sortItems id payees)
  where (payers, payees) = Map.partition (> 0) m
        sortItems f p = sortBy (f (comparing snd)) $ Map.toList p

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON file = BS.writeFile file . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "resources/HW05/dog-original.jpg"
                        "resources/HW05/dog.jpg"
                        "resources/HW05/transactions.json"
                        "resources/HW05/victims.json"
                        "resources/HW05/new-ids.json"
                        "resources/HW05/new-transactions.json"
  putStrLn crim

-- ghc -o foo CIS194/ClaudioN/HW05.hs -main-is CIS194.ClaudioN.HW05



--
-- REPL play
--
{--
 getSecret "../resources/HW05/dog-original.jpg" "../resources/HW05/dog.jpg"
   => "Haskell Is Great!"

:{
do
  key <- getSecret "../resources/HW05/dog-original.jpg" "../resources/HW05/dog.jpg"
  putStrLn "decrypting..."
  decryptWithKey key "../resources/HW05/victims.json"
:}

 parseFile "../resources/HW05/victims.json" :: IO (Maybe [TId])

 getBadTs "../resources/HW05/victims.json" "../resources/HW05/victims.json" => Nothing
 getBadTs "../resources/HW05/transactions.json" "../resources/HW05/transactions.json" => Nothing
 getBadTs "../resources/HW05/victims.json" "../resources/HW05/transactions.json" => Just [...]

:{
do
  Just trans <- getBadTs "../resources/HW05/victims.json" "../resources/HW05/transactions.json"
  return $ getFlow trans
:}

:{
do
  Just trans <- getBadTs "../resources/HW05/victims.json" "../resources/HW05/transactions.json"
  return $ getCriminal $ getFlow trans
:}

:{
do
undoTs (Map.fromList [ ("Haskell Curry", -20)
       , ("Simon Peyton Jones", 10)
       , ("Foo", 0)
       , ("Bar", 17)
       , ("Baz", -7)
       ])
       ["a", "b", "c", "d", "e", "f"]
:}

--}
