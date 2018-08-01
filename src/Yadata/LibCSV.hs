{-# LANGUAGE OverloadedStrings #-}

module Yadata.LibCSV
    ( read2DoubleMaybe
    , backFillNothings
    , backFillNothings2S
    , backFillNothings2Doubles
    , read2UTCTimeMaybe
    , readClean2UTCTime
    , getColumnInCSV
    , getColumnInCSVEither
    , delColumnInCSV
    , removeAtIndexList
    ) where

import Data.List
import Data.Time
import Text.CSV
import Text.Read (readMaybe)
import Data.Maybe

{-|
   Converts a number in String to Double
-}
read2DoubleMaybe :: String -> Maybe Double
read2DoubleMaybe x = readMaybe x :: Maybe Double


backFillNothings :: Num a => [Maybe a] -> [Maybe a]
backFillNothings [] = []
backFillNothings (x:[]) = [x]
backFillNothings (x:y:[]) = if (isNothing y) then (x:x:[]) else  (x:y:[])
backFillNothings (x:y:rest) = if (isJust x && isNothing y) then backFillNothings (x:x:rest) else  (x:( backFillNothings (y:rest) ))


backFillNothings2S :: (Num a, Eq a) => [Maybe a] -> [Maybe a]
backFillNothings2S values = backfilled
      where
         backfilled_ = backFillNothings values
         backfilled  = if any (==Nothing) backfilled_ 
                            then reverse $ backFillNothings (reverse backfilled_)
                            else backfilled_

            
backFillNothings2Doubles :: [Maybe Double] -> [Double]
backFillNothings2Doubles values = catMaybes $ backFillNothings2S values


{-|
   Converts a date in String to UTCTime
-}

read2UTCTimeMaybe :: String -> String -> Maybe UTCTime
read2UTCTimeMaybe format x = parseTimeM True defaultTimeLocale format x :: Maybe UTCTime


readClean2UTCTime :: String -> [String] -> [UTCTime]
readClean2UTCTime format x = utcTimes
      where
            maybeUTCTimes = fmap (read2UTCTimeMaybe format) x
            utcTimes = if any (== Nothing) maybeUTCTimes 
                           then []
                           else catMaybes maybeUTCTimes

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


removeAtIndex :: Int -> [a] -> [a]
removeAtIndex i [] = []
removeAtIndex i list =
      if (i > length list || i < 0)
            then list
            else (init alist) ++ blist
                     where (alist, blist) = splitAt (i + 1) list

removeAtIndexList :: [Int] -> [a] -> [a]
removeAtIndexList [] list = list
removeAtIndexList _    [] = []
removeAtIndexList (i:idx) list = removeAtIndexList idx alist
      where
            alist = removeAtIndex i list


delColumnInCSV :: CSV -> String -> [[Field]]
delColumnInCSV acsv columnName =
      map (removeAtIndex columnIndex') records
      where
         columnIndex = findColumnIndexInCSV acsv columnName
         columnIndex' = fromInteger $ either (\_ -> -1) id columnIndex
         nfieldsInFile = length $ head acsv
         records = tail $ filter (\record -> nfieldsInFile == length record) acsv
