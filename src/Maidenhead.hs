module Maidenhead (maidenheadToWgs84) where

import Data.Char (toUpper, ord)
import Data.List (transpose)

-- |Minimum and maximum symbol for each position in Maidenhead system
alphabet = zip (cycle "A0") ('R' : cycle "9X")

-- |chunksOf n splits a list into length-n pieces. The last piece will
-- be shorter if n does not evenly divide the length of the list.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = case splitAt n xs of
  (a,[]) -> [a]
  (a,b)  -> a : chunksOf n b

-- |Takes (min,max) of given character position, the character
-- x, and the accumulator. Converts the given position to a value
-- between 0 and 1 and combines it with the accumulator.
charToFloat :: Fractional a => ((Char, Char), Char) -> a -> a
charToFloat ((min,max),x) acc = (acc + fromIntegral (ord (toUpper x) - ord min)) / scale
  where scale = fromIntegral (ord max - ord min + 1)

-- |Converts Maidenhead coordinate string to latitude and longitude in
-- WGS84 degrees.
maidenheadToWgs84 :: (Fractional a) => String -> (a, a)
maidenheadToWgs84 s = ((latRaw-0.5)*180, (lonRaw-0.5)*360)
  where [lonRaw, latRaw] = map toPos $ transpose $ chunksOf 2 s
        toPos = foldr charToFloat (1/2) . zip alphabet -- 0.5 gets the centre of a square
