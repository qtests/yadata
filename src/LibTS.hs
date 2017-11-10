{-# LANGUAGE OverloadedStrings #-}

-- Time Series
-- #############

module LibTS
( 
    getDateTimeIntervalTS,
    isAWorkingDay,
    alignTS,
    alignTSIndex,
    backFillTS,
    alignAndBackfillTS,
    TS(..),
    createTSRaw,
    createTSEither,
    writeFileTS,
    readFileTS
) where

import Data.Time
import Data.Time.Calendar.WeekDate
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Either
import Text.CSV
import Control.Arrow (second)

import LibCSV
-- ###########################################################################

isAWorkingDay::UTCTime -> Bool
isAWorkingDay x = 
    let myWeekDay = (toWeekDate . utctDay) x
        (_, _, aWeekDay) = myWeekDay
    in aWeekDay < 6

-- https://two-wrongs.com/haskell-time-library-tutorial

getDateTimeInterval :: [UTCTime] -> [UTCTime]
getDateTimeInterval timeIndex = 
    fmap (\x -> addUTCTime (24*60*60*( fromRational x)) mint) [0 .. interval]
    where
        mint = minimum timeIndex
        maxt = maximum timeIndex
        interval = (toRational (diffUTCTime maxt mint)) / (60*60*24)


getDateTimeIntervalTS' :: Num a => [(UTCTime, a)] -> [UTCTime]
getDateTimeIntervalTS' tseries = 
    getDateTimeInterval dates
    where
        (dates, _) = unzip tseries


getDateTimeIntervalTS :: Num a => Either String [(UTCTime, a)] -> Either String [UTCTime]
getDateTimeIntervalTS tseries = do
    tseries' <- tseries
    return $ getDateTimeIntervalTS' tseries'


-- https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/containers-0.3.0.0/Data-Map.html
alignTS' :: Num a => [UTCTime] -> [(UTCTime, a)] -> [(UTCTime, Maybe a)]
alignTS' [] ts = alignTS' ( filter isAWorkingDay $ getDateTimeIntervalTS' ts) ts 
alignTS' _ [] = []
alignTS' idx ts = zip idx' combinedValues
     where   tvMap = foldl (\mm (key, value) -> Map.insert key value mm) Map.empty ts
             idx' = sort idx
             combinedValues = map (\v -> Map.lookup v tvMap) idx'                          


alignTSIndex :: Num a => Either String [UTCTime] -> Either String [(UTCTime, a)] -> Either String [(UTCTime, Maybe a)]                    
alignTSIndex idx ts = do 
    ind <- idx
    dta <- ts
    return $ alignTS' ind dta


alignTS :: Num a => Either String [(UTCTime, a)] -> Either String [(UTCTime, Maybe a)]                    
alignTS ts = do 
    dta <- ts
    return $ alignTS' [] dta


backFillTS' :: Num a => [Maybe a] -> [Maybe a]
backFillTS' [] = []
backFillTS' (x:[]) = [x]
backFillTS' (x:y:[]) = if (isNothing y) then (x:x:[]) else  (x:y:[])
backFillTS' (x:y:rest) = if (isJust x && isNothing y) then backFillTS' (x:x:rest) else  (x:( backFillTS' (y:rest) ))


backFillTS :: Num a => Either String [(UTCTime, Maybe a)] -> Either String [(UTCTime, Maybe a)] 
backFillTS ts = do 
    ts' <- ts
    let (tsIndex, values) = unzip ts'
    return $ zip tsIndex (backFillTS' values)
    

alignAndBackfillTS :: (Eq a, Num a) => [(UTCTime, a)] -> Either String [(UTCTime, a)]
alignAndBackfillTS ts = do
    let tsa =  alignTS' [] ts 
    let (tsIndex, values) = unzip tsa
    let valuesB = backFillTS' values
    let values' = if any (== Nothing) valuesB 
                      then reverse (backFillTS' $ reverse valuesB ) else valuesB
    if all (== Nothing) values' 
        then Left "No data!"
        else return $ zip tsIndex ( catMaybes values' )

-- TS -------------------------------------------------------------------------------------------
-- **********************************************************************************************

data TS a = TS [UTCTime] [a]

createTSRaw :: (Eq a, Num a) => [UTCTime] -> [a] -> TS a
createTSRaw times values = TS abtimes abvalues
    where 
        ab = alignAndBackfillTS (zip times values)
        (abtimes, abvalues) = if (isLeft ab) then ([], []) else unzip (concat $ rights [ab])

        
createTSEither :: (Eq a, Num a) => Either String [(UTCTime, a)]  -> TS a
createTSEither ts = TS abtimes abvalues
    where
        ts1 = if (isLeft ts) then [] else (concat $ rights [ts])
        ab = alignAndBackfillTS ts1 
        (abtimes, abvalues) = if (isLeft ab) then ([], []) else unzip (concat $ rights [ab])


instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows
      where rows = zipWith (\x y -> mconcat [show x," | ",show y,"\n"] ) times values


writeFileTS :: (Show a) => FilePath -> TS a -> IO ()
writeFileTS path (TS times values) =
    writeFile path tsString
    where
        -- formatTime defaultTimeLocale "%F %T (%Z)" x
        tsString = mconcat $ ["Date,Value\n"] ++ zipWith (\x y -> mconcat [show x,",",show y,"\n"] ) times values


readFileTS :: FilePath -> IO (TS Double)
readFileTS path = do
    let tstext = readFile path
    txt <- tstext
    let ptxt = parseCSV path txt
    let date =  either (\_-> []) (\x-> fmap (read2UTCTime "%Y-%m-%d %H:%M:%S %Z") x) (getColumnInCSV ptxt "Date")
    let value = either (\_-> []) (\x-> fmap read2Double x) (getColumnInCSV ptxt "Value")
    return $ TS date value
 

-- XTS ?

-- ts <- priceTimeSeries "IBM"
-- let ts1 = fmap (take 20) ts
-- let tx = createTSEither ts1
-- print tx