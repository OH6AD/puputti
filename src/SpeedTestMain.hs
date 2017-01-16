module Main where

import SpeedTest
import PupuTools
import PupuCsv
import System.Environment
import qualified Data.Map.Strict as M

main = do
  [csvPath, from, to] <- getArgs
  net <- readPupuCsv csvPath
  results <- runTest (findBy name net from) (findBy name net to)
  putStr $ unlines $ map show $ M.toList results
