{-# LANGUAGE OverloadedStrings #-}
module SpeedTest where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Environment
import System.Process
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString, pack)

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
  either (fail.show) return $ A.parseOnly keyVals $ pack out

keyVal :: A.Parser (ByteString, ByteString)
keyVal = do
  A.skipWhile $ A.inClass " \t"
  k <- A.takeWhile $ A.notInClass ":\r\n"
  A.string ": "
  v <- A.takeWhile $ A.notInClass "\r\n"
  return (k,v)

keyVals :: A.Parser (M.Map ByteString ByteString)
keyVals = do
  allPairs <- keyVal `A.sepBy` A.many' A.endOfLine
  return $ M.fromList allPairs -- Keep only recent pairs
