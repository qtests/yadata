{-# LANGUAGE OverloadedStrings #-}

module LibCSV
    ( read2Double
    , read2UTCTime
    , getColumnInCSV
    ) where

import Text.CSV
import Data.List
import Data.Time
import qualified Data.CSV.Conduit as C hiding (CSV)
import Data.CSV.Conduit.Conversion
import Data.Text (Text)
import LibYahoo (getYahooData)
import Control.Monad (mzero)

{-|
   Converts a number in String to Double
-}
read2Double :: String -> Double
read2Double x = read x :: Double

{-|
   Converts a date in String to UTCTime
-}
read2UTCTime :: String -> String -> UTCTime
read2UTCTime format x = parseTimeOrError True defaultTimeLocale format x :: UTCTime


{-|
   Applies a function to a column (specified by a Sring) in a CSV value
   Returns (Left errorMessage) or (Right b)
-}
applyToColumnInCSV :: ([String] -> b) -> CSV -> String -> Either String b
applyToColumnInCSV func csv column =
    either
    Left
    (Right . func . elements) columnIndex
    where
      columnIndex = getColumnIndexInCSV csv column
      nfieldsInFile = length $ head csv
      records = tail $ filter (\record -> nfieldsInFile == length record) csv
      elements ci = map (\record -> genericIndex record ci) records


{-|
   Gets a column from a CSV value.
   Returns (Left errorMessage) or (Right index)
-}
getColumnIndexInCSV :: CSV -> String -> Either String Integer
getColumnIndexInCSV csv columnName =
    case lookupResponse of
          Nothing -> Left "The column does not exist in this CSV!"
          Just x -> Right (fromIntegral x)
    where
      -- This line does the lookup to see if column is in our CSV
      lookupResponse = findIndex (== columnName) (head csv)



getColumnInCSV ::  Either a CSV -> String -> Either String [String]
getColumnInCSV csv columnName =
    either
       (\r -> Left "Error reading CSV!" )
       (\x -> applyToColumnInCSV id x columnName) csv


------------------------------------------------------------------------------------------------------

data YahooData = YahooData
  { yahooDataDate :: !Text
  , yahooDataOpen :: !Text
  , yahooDataHigh :: !Text
  , yahooDataLow :: !Text
  , yahooDataClose :: !Text
  , yahooDataAdjClose :: !Text
  , yahooDataVolume :: !Text
  } deriving (Show, Eq)

instance FromRecord YahooData where
    parseRecord v
        | length v == 7 = YahooData <$>
                          v .! 0 <*>
                          v .! 1 <*>
                          v .! 2 <*>
                          v .! 3 <*>
                          v .! 4 <*>
                          v .! 4 <*>
                          v .! 6
        | otherwise     = mzero

instance ToRecord YahooData where
    toRecord (YahooData yahooDataDate yahooDataOpen  yahooDataHigh  yahooDataLow  yahooDataClose  yahooDataAdjClose yahooDataVolume) = record [
        toField yahooDataDate, toField yahooDataOpen, toField  yahooDataHigh, toField  yahooDataLow, toField  yahooDataClose, toField  yahooDataAdjClose, toField yahooDataVolume]

readToType = do
   yd <- getYahooData
   return undefined
