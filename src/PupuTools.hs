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

-- |Produce link pairs ready for speed testing etc.
linksToIps :: [PupuLink] -> [(Text,Text,Text)]
linksToIps xs = [ (linkName, ipv4 a, ipv4 b)
                | PupuLink{..} <- xs
                , station <- stations
                , (a,b) <- [(ap,station),(station,ap)] -- Test both directions
                , let linkName = T.concat [name a, "-", name b]
                ]
