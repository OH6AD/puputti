{-# LANGUAGE OverloadedStrings #-}
module SpeedTest where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as A hiding (Result)
import Data.ByteString (ByteString)
import Data.List
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import PupuHosts
import RunAndParse
import Result

type ResultInc = Result -> Result

runTest :: PupuHost -> PupuHost -> IO [Result]
runTest a b = do
  pw <- getEnv "SSHPASS"
  let initialResult = Result (name a) (name b) (-1) (-1) (-1) "status missing" 
  out <- runAndParse (results initialResult) "sshpass"
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
result :: Result -> Parser Result
result initialResult = do
  xs <- line `sepBy` endOfLine
  endOfLine
  endOfLine
  -- What follows is a functional orgasm. We construct Result by
  -- folding a list of ResultIncs starting from a initial dummy state.
  return $ foldl' (\x f -> f x) initialResult xs
  where line = rxAvgLine <|> rxSizeLine <|> lostPacketsLine <|> statusLine <|> garbageLine

results :: Result -> Parser [Result]
results r = many $ result r

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
  return $ \r -> r{ resultRxAvg = a }

lostPacketsLine :: Parser ResultInc
lostPacketsLine = do
  key "lost-packets"
  a <- decimal
  return $ \r -> r{ resultLostPackets = a }

statusLine :: Parser ResultInc
statusLine = do
  key "status"
  a <- A.takeWhile $ notInClass "\r\n"
  return $ \r -> r{ resultStatus = T.decodeUtf8With T.lenientDecode a }

rxSizeLine :: Parser ResultInc
rxSizeLine = do
  key "rx-size"
  a <- decimal
  return $ \r -> r{ resultRxSize = a }

-- |Garbage line is a line which we don't understand. Returns result untouched.
garbageLine :: Parser ResultInc
garbageLine = do
  takeWhile1 $ notInClass "\r\n"
  return id
