{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module PupuTools where

import PupuCsv
import Data.Maybe
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

data PupuLink = PupuLink { ap       :: PupuRow
                         , stations :: [PupuRow]
                         } deriving (Show)

-- |Group rows to ap-station lists and drop hosts which have no
-- AP binding.
links :: [PupuRow] -> [PupuLink]
links xs = [ PupuLink a ss
           | a <- aps
           , let ss = [ s
                      | s <- rest
                      , connects s == Just (name a)
                      ]
           , not $ null ss
           ]
  where (rest,aps) = partition (isJust.connects) xs

-- |Produce link pairs ready for speed testing etc.
linksToIps :: [PupuLink] -> [(Text,Text,Text)]
linksToIps xs = [ (linkName, ipv4 a, ipv4 b)
                | PupuLink{..} <- xs
                , station <- stations
                , (a,b) <- [(ap,station),(station,ap)] -- Test both directions
                , let linkName = T.concat [name a, "-", name b]
                ]
