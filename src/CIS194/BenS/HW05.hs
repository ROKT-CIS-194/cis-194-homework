{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module CIS194.BenS.HW05 where

import Control.Arrow                  ((***))
import Data.Bits                      (xor)
import Data.ByteString.Lazy           (ByteString)
import Data.List                      (partition, sortBy)
import Data.Map.Strict                (Map)
import Data.Ord                       (comparing)
import Data.Semigroup                 (Max(..), option)
import System.Environment             (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set

import CIS194.BenS.HW05.Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret secPath origPath =
  (BS.pack . filter (/= 0)) <$> readKey
  where
    readKey = BS.zipWith xor <$> BS.readFile secPath <*> BS.readFile origPath

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path =
  BS.readFile (path ++ ".enc") >>= BS.writeFile path . decrypt
  where
    decrypt = BS.pack . BS.zipWith xor (BS.cycle key)

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = fmap decode . BS.readFile

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsPath transactionsPath = do
  victimsM <- fmap Set.fromList <$> parseFile victimsPath
  transactionsM <- parseFile transactionsPath
  return $ do
    victims <- victimsM
    filter (\x -> Set.member (tid x) victims) <$> transactionsM

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = Map.unionsWith (+) . map f
  where
    f (Transaction from to amount _tid) =
      Map.fromList [(from, -amount), (to, amount)]

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = extractName . Map.foldrWithKey f mempty
  where
    f nm amount = mappend (pure (Max (amount, nm)))
    extractName = option (error "No transactions!") (\(Max (_, nm)) -> nm)

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs amounts tids' = go tids' payers payees
  where
    go (t:tids) ((payer, debt):as) ((payee, owed):bs) =
      let mkT amount = Transaction payer payee amount t
      in case compare debt (-owed) of
           EQ -> mkT debt    : go tids as bs
           LT -> mkT debt    : go tids as ((payee, debt+owed):bs)
           GT -> mkT (-owed) : go tids ((payer, debt+owed):as) bs
    go _ _ _ = []

    (payers, payees) = (sortBy compareDesc *** sortBy compareAsc)
                     . partition ((>= 0) . snd)
                     . Map.toList
                     $ Map.filter (/= 0) amounts

    compareDesc = comparing (negate . snd)
    compareAsc = comparing snd

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path = BS.writeFile path . encode

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
