{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module CIS194.NavinK.HW05 where

import Data.Char
import Data.Word
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Data.Bits (xor)

import CIS194.NavinK.HW05.Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret f1 f2 = do
  f1_contents <- BS.readFile f1
  f2_contents <- BS.readFile f2
  let xor_f1_f2 = BS.zipWith xor f1_contents f2_contents
  return $ BS.pack $ filter (/= 0) xor_f1_f2

-- Haskell Is Great!

-- Exercise 2 -----------------------------------------

-- Convert a Char to a Word8

c2w :: Char -> Word8
c2w = fromIntegral . ord

f :: ByteString -> ByteString -> Int
f bs1 bs2 = fromIntegral(BS.length bs1) `div` fromIntegral(BS.length bs2) + 1

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key filePath = do
  ciphertext <- BS.readFile (concat [filePath, ".enc"])
  let n = f ciphertext key
      lengthenedKey = BS.concat (replicate n key)
      plainText = BS.pack $ BS.zipWith xor ciphertext lengthenedKey
  BS.writeFile filePath plainText

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = undefined

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs = undefined

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = undefined

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

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
