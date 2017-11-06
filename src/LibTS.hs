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
    alignAndBackfillTS
) where

import Data.Time
import Data.Time.Calendar.WeekDate
import qualified Data.Map as Map
import Control.Arrow (second)
import Data.List
import Data.Maybe

-- ###########################################################################

isAWorkingDay::UTCTime -> Bool
isAWorkingDay x = 
    let myWeekDay = (toWeekDate . utctDay) x
        (_, _, aWeekDay) = myWeekDay
    in aWeekDay < 6

-- https://two-wrongs.com/haskell-time-library-tutorial

getDateTimeIntervalTS' :: Num a => [(UTCTime, a)] -> [UTCTime]
getDateTimeIntervalTS' tseries = 
    fmap (\x -> addUTCTime (24*60*60*( fromRational x)) mint) [0 .. interval]
    where
        (dates, _) = unzip tseries
        mint = minimum dates
        maxt = maximum dates
        interval = (toRational (diffUTCTime maxt mint)) / (60*60*24)


getDateTimeIntervalTS :: (Ord a, Num a) => Either String [(UTCTime, a)] -> Either String [UTCTime]
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
    

alignAndBackfillTS :: (Eq a, Num a) => Either String [(UTCTime, a)] -> Either String [(UTCTime, a)]
alignAndBackfillTS ts = do
    dta <- ts
    let tsa =  alignTS' [] dta 
    let (tsIndex, values) = unzip tsa
    let valuesB = backFillTS' values
    let values' = if any (== Nothing) valuesB 
                      then reverse (backFillTS' $ reverse valuesB ) else valuesB
    if all (== Nothing) values' 
        then Left "No data!"
        else return $ zip tsIndex ( catMaybes values' )

-- XTS ?

-- ts <- priceTimeSeries "IBM"
-- let ts1 = fmap (take 20) ts
-- let tsa = alignTS ts1
-- let tsb = backFillTS tsa