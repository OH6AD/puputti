{-# LANGUAGE OverloadedStrings #-}
module SpeedTest where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as A hiding (Result)
import Data.ByteString.Char8 (ByteString)
import Data.Foldable
import System.Environment
import qualified Data.Text as T

import PupuHosts
import RunAndParse

data Result = Result { lostPackets :: Int
                     , rxAvg       :: Double
                     , rxSize      :: Int
                     , status      :: ByteString
                     } deriving (Show)

type ResultInc = Result -> Result

emptyResult = Result undefined undefined undefined undefined

runTest :: PupuHost -> PupuHost -> IO [Result]
runTest a b = do
  pw <- getEnv "SSHPASS"
  out <- runAndParse results "sshpass"
         ["-e", "ssh", "test@" ++ T.unpack (ipv4 a), "/tool", "bandwidth-test"
         ,"address=" ++ T.unpack (ipv4 b), "user=test", "password=" ++ pw
         ,"duration=20"
         ]
  either (fail.show) return out

key :: ByteString -> Parser ()
key k = do
  skipWhile $ inClass " \t"
  string k
  string ": "
  return ()

-- |Parses single result which is list of key-value pairs and ends with
-- a blank line.
result :: Parser Result
result = do
  xs <- line `sepBy` endOfLine
  endOfLine
  endOfLine
  -- What follows is a functional orgasm. We construct Result by
  -- folding a list of ResultIncs starting from a initial dummy state.
  return $ foldl' (\x f -> f x) emptyResult xs
  where line = rxAvgLine <|> rxSizeLine <|> lostPacketsLine <|> statusLine <|> garbageLine

results :: Parser [Result]
results = many result

-- |Parses a value with a possiqle SI multiplier (kilo, mega, giga)
doubleWithSiPrefix :: Parser Double
doubleWithSiPrefix = do
  n <- double
  m <- (char 'k' >> pure 1e3) <|> (char 'M' >> pure 1e6) <|> (char 'G' >> pure 1e9) <|> pure 1
  return $ n*m

rxAvgLine :: Parser ResultInc
rxAvgLine = do
  key "rx-10-second-average"
  a <- doubleWithSiPrefix
  string "bps"
  return $ \r -> r{ rxAvg = a }

lostPacketsLine :: Parser ResultInc
lostPacketsLine = do
  key "lost-packets"
  a <- decimal
  return $ \r -> r{ lostPackets = a }

statusLine :: Parser ResultInc
statusLine = do
  key "status"
  a <- A.takeWhile $ notInClass "\r\n"
  return $ \r -> r{ status = a }

rxSizeLine :: Parser ResultInc
rxSizeLine = do
  key "rx-size"
  a <- decimal
  return $ \r -> r{ rxSize = a }

-- |Garbage line is a line which we don't understand. Returns result untouched.
garbageLine :: Parser ResultInc
garbageLine = do
  takeWhile1 $ notInClass "\r\n"
  return id
