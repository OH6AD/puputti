module Main where

import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import System.Environment

import PupuHosts
import PupuTools
import SpeedTest

main = do
  [csvPath, from, to] <- getArgs
  net <- readPupuCsv csvPath
  results <- runTest (findBy name net from) (findBy name net to)
  putStr $ unlines $ map show results

-- |Shorthand for searching by given field
findBy f hosts x = fromJust $ find (\y -> f y == T.pack x) hosts
