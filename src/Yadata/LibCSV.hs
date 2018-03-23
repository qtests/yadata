{-# LANGUAGE OverloadedStrings #-}

module Yadata.LibCSV
    ( read2Double
    , read2UTCTimeMaybe
    , getColumnInCSV
    , getColumnInCSVEither
    , delColumnInCSV
    ) where

import Data.List
import Data.Time
import Text.CSV

{-|
   Converts a number in String to Double
-}
read2Double :: String -> Double
read2Double x = read x :: Double

{-|
   Converts a date in String to UTCTime
-}
-- read2UTCTime :: String -> String -> UTCTime
-- read2UTCTime format x = parseTimeOrError True defaultTimeLocale format x :: UTCTime

read2UTCTimeMaybe :: String -> String -> Maybe UTCTime
read2UTCTimeMaybe format x = parseTimeM True defaultTimeLocale format x :: Maybe UTCTime

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
      columnIndex = findColumnIndexInCSV csv column
      nfieldsInFile = length $ head csv
      records = tail $ filter (\record -> nfieldsInFile == length record) csv
      elements ci = map (\record -> genericIndex record ci) records

{-|
   Gets a column from a CSV value.
   Returns (Left errorMessage) or (Right index)
-}
findColumnIndexInCSV :: CSV -> String -> Either String Integer
findColumnIndexInCSV csv columnName =
    case lookupResponse of
          Nothing -> Left "The column does not exist in this CSV!"
          Just x  -> Right (fromIntegral x)
    where
      -- This line does the lookup to see if column is in our CSV
      lookupResponse = findIndex (== columnName) (head csv)


findColumnsIndicesInCSV :: CSV -> String -> [Integer]
findColumnsIndicesInCSV csv columnsNames =
      map fromIntegral indices
      where
            indices = findIndices (== columnsNames) (head csv)

-- each n = map head . takeWhile (not . null) . iterate (drop n)
getColumnInCSV :: CSV -> String -> Either String [String]
getColumnInCSV csv columnName =
      applyToColumnInCSV id csv columnName


getColumnInCSVEither :: Either a CSV -> String -> Either String [String]
getColumnInCSVEither csv columnName = do
      either (\_ -> Left "Error reading CSV!" )
             (\x -> applyToColumnInCSV id x columnName) csv


removeAt :: Int -> [a] -> [a]
removeAt i [] = []
removeAt i list =
      if (i > length list || i < 0)
            then list
            else (init alist) ++ blist
                     where (alist, blist) = splitAt (i + 1) list


delColumnInCSV :: CSV -> String -> [[Field]]
delColumnInCSV acsv columnName =
      map (removeAt columnIndex') records
      where
         columnIndex = findColumnIndexInCSV acsv columnName
         columnIndex' = fromInteger $ either (\_ -> -1) id columnIndex
         nfieldsInFile = length $ head acsv
         records = tail $ filter (\record -> nfieldsInFile == length record) acsv
