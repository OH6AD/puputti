-- |Reads Pupu CSV files produced by Google Spreadsheets
{-# LANGUAGE OverloadedStrings #-}
module PupuCsv where

import Data.ByteString.Lazy as B (readFile)
import Data.Csv
import Control.Applicative
import Data.Text (Text)
import Data.Vector (toList)

data PupuHost = PupuHost { name      :: !Text
                         , lan       :: !Text
                         , ipv4      :: !Text
                           -- skip ipMissing
                           -- skip ipv4Old
                         , connects  :: !(Maybe Text)
                         , standard  :: !Text
                         , power     :: !(Maybe Int)
                         } deriving (Show)

instance FromRecord PupuHost where
    parseRecord v = PupuHost <$>
                    v .! 0 <*>
                    v .! 1 <*>
                    v .! 2 <*>
                    (apfy <$> v .! 5) <*>
                    v .! 6 <*>
                    v .! 7
      where apfy (Just "AP") = Nothing
            apfy x           = x

readPupuCsv :: FilePath -> IO [PupuHost]
readPupuCsv f = either error toList . decode HasHeader <$> B.readFile f
