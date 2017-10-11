{-# LANGUAGE OverloadedStrings #-}

module LibCSV
    ( read2Double
    , read2UTCTime
    , getColumnInCSV
    ) where

import Text.CSV
import Data.List
import Data.Time


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
       (\_ -> Left "Error reading CSV!" ) 
       (\x -> applyToColumnInCSV id x columnName) csv

    