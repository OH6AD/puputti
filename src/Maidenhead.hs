module Maidenhead ( maidenheadToWgs84
                  , maidenheadToWgs84rad
                  , distance
                  , maidenheadDistance
                  ) where

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
charToFloat :: Fractional a => ((Char, Char), Char) -> Maybe a -> Maybe a
charToFloat _ Nothing = Nothing
charToFloat ((min,max),x') (Just acc)
  | x < min || x > max = Nothing
  | otherwise = Just $ (acc + fromIntegral (ord x - ord min)) / scale
  where scale = fromIntegral $ ord max - ord min + 1
        x = toUpper x'

-- |Converts Maidenhead coordinate string to latitude and longitude in
-- WGS84 (in semicircles i.e. 180°)
maidenheadToWgs84sc :: (Fractional a) => String -> Maybe (a, a)
maidenheadToWgs84sc s = latLon <$> mapM toPos (transpose $ chunksOf 2 s)
   where
     latLon [lonRaw, latRaw] = ((latRaw-0.5), (lonRaw-0.5)*2)
     toPos = foldr charToFloat middle . zip alphabet
     middle = Just (1/2) -- 0.5 gets the centre of a square

-- |Converts Maidenhead coordinate string to latitude and longitude in
-- WGS84 degrees.
maidenheadToWgs84 :: Fractional t => String -> Maybe (t, t)
maidenheadToWgs84 s = (\(a,b) -> (180*a,180*b)) <$> maidenheadToWgs84sc s

-- |Converts Maidenhead coordinate string to latitude and longitude in
-- WGS84 radians.
maidenheadToWgs84rad :: Floating t => String -> Maybe (t, t)
maidenheadToWgs84rad s = (\(a,b) -> (pi*a,pi*b)) <$> maidenheadToWgs84sc s

-- |Calculate distance in kilometres between two points (in
-- radians) using Haversine formula
-- https://en.wikipedia.org/wiki/Haversine_formula
distance (φ1, λ1) (φ2, λ2) = d * asin (sqrt (sin² ((φ2-φ1)/2) + cos φ1 * cos φ2 * sin² ((λ2-λ1)/2)))
  where d = 12735 -- Earth's diameter
        sin² = (**2) . sin

-- |Calculates distance in kilometres between two points in Maidenhead
maidenheadDistance :: Floating a => String -> String -> Maybe a
maidenheadDistance a b = distance <$>
                         maidenheadToWgs84rad a <*>
                         maidenheadToWgs84rad b
