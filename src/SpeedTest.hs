module SpeedTest where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Environment
import System.Process
import Text.Parsec
import Text.Parsec.String

import PupuHosts

runTest a b = do
  pw <- getEnv "SSHPASS"
  out <- readProcess
    "sshpass"
    ["-e", "ssh", "test@" ++ T.unpack (ipv4 a), "/tool", "bandwidth-test"
    ,"address=" ++ T.unpack (ipv4 b), "user=test", "password=" ++ pw
    ,"duration=20"
    ]
    ""
  either (fail.show) return $ parse keyVals "" out

keyVal :: Parser (String, String)
keyVal = do
  many $ oneOf " \t"
  k <- many $ noneOf ":\r\n"
  string ": "
  v <- many $ noneOf "\r\n"
  return (k,v)

keyVals :: Parser (M.Map String String)
keyVals = do
  allPairs <- keyVal `sepEndBy` many endOfLine
  return $ M.fromList allPairs -- Keep only recent pairs
