{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module PupuTools where

import PupuCsv
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

data PupuLink = PupuLink { ap       :: PupuHost
                         , stations :: [PupuHost]
                         } deriving (Show)

-- |Group rows to ap-station lists and drop hosts which have no
-- AP binding.
links :: [PupuHost] -> [PupuLink]
links xs = [ PupuLink{..}
           | ap <- filter (isNothing.connects) xs
           , let stations = [ s
                            | s <- xs
                            , connects s == Just (name ap)
                            ]
           , not $ null stations
           ]

-- |Produce link pairs for speed testing. Returns array of arrays
-- where the outer array can be processed simultaneously but inner
-- arrays must be done sequentially because they share the same AP and
-- share the available bandwidth.
speedTestIps :: [PupuLink] -> [[(Text, Text)]]
speedTestIps xs = [ [ (ipv4 a, ipv4 b)
                    | station <- stations
                    , (a,b) <- [(ap,station),(station,ap)] -- Test both directions
                    ]
                  | PupuLink{..} <- xs
                  ]
