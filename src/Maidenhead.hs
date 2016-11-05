module Maidenhead ( maidenheadToWgs84
                  , maidenheadDistance
                  , maidenheadMaxDistance
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

-- |Centre of a square.
centre :: Fractional a => [a]
centre = [1/2,1/2]

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

-- |Converts Maidenhead coordinate string to latitude and longitude
-- starting from lower left corner of the map. Takes point as a an
-- argument which can be [0.5,0.5] if you want to find the centre of
-- the square.
maidenheadToFrac :: Fractional a => [a] -> String -> Maybe [a]
maidenheadToFrac point s = sequence $ zipWith toPos point (transpose $ chunksOf 2 s)
   where toPos middle = foldr charToFloat (Just middle) . zip alphabet

-- |Converts Maidenhead coordinate string to latitude and longitude in
-- WGS84 degrees. Gets the center of the square
maidenheadToWgs84 :: Fractional t => String -> Maybe (t, t)
maidenheadToWgs84 s = (\[a,b] -> (180*(b-0.5),360*(a-0.5))) <$> maidenheadToFrac centre s

-- |Converts Maidenhead coordinate string to latitude and longitude in
-- WGS84 radians. Longitude is not normalized to Greenwich because
-- distance and bearing algorithms don't need normalization.
maidenheadToRad :: Floating t => [t] -> String -> Maybe (t, t)
maidenheadToRad point s = (\[a,b] -> (pi*(b-0.5),2*pi*a)) <$> maidenheadToFrac point s

-- |Calculate distance in kilometres between two points (in
-- radians) using Haversine formula
-- https://en.wikipedia.org/wiki/Haversine_formula
distance (φ1, λ1) (φ2, λ2) = d * asin (sqrt (sin² ((φ2-φ1)/2) + cos φ1 * cos φ2 * sin² ((λ2-λ1)/2)))
  where d = 12735 -- Earth's diameter
        sin² = (**2) . sin

-- |Calculates distance in kilometres between two points in Maidenhead
-- measured from the centres of both squares.
maidenheadDistance :: Floating a => String -> String -> Maybe a
maidenheadDistance a b = distance <$>
                         maidenheadToRad centre a <*>
                         maidenheadToRad centre b

-- |Calculates distance in kilometres between two points in Maidenhead
-- format. Returns maximum distance possible between those
-- squares. Because the station may be in any point of the square, we
-- calculate the worst possible distance by measuring the maximum
-- distance between any of the corners of both squares.
maidenheadMaxDistance :: RealFloat t => String -> String -> Maybe t
maidenheadMaxDistance a b = maximum <$> sequence
                            [ distance <$>
                              maidenheadToRad [x1,y1] a <*>
                              maidenheadToRad [x2,y2] b
                            | x1 <- [0,1]
                            , y1 <- [0,1]
                            , x2 <- [0,1]
                            , y2 <- [0,1]
                            ]

-- TODO maidenheadMinDistance is not yet implemented. It is not needed
-- in my application and it is non-trivial to implement because there
-- may be enclaves and small box near the edge of a bigger box. Feel
-- free to implement.
