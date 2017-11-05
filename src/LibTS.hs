{-# LANGUAGE OverloadedStrings #-}

-- Time Series
-- #############

module LibTS
( 
    getDateTimeInterval,
    isAWorkingDay,
    alignTS,
    backFillTS
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

getDateTimeInterval' :: Num a => [(UTCTime, a)] -> [UTCTime]
getDateTimeInterval' tseries = []


getDateTimeInterval :: (Ord a, Num a) => Either String [(UTCTime, a)] -> Either String [UTCTime]
getDateTimeInterval tseries = do
    let mint = fmap (fst . minimum) tseries
    let maxt = fmap (fst . maximum) tseries
    let timediff = fmap (\x -> (toRational x) / (60*60*24) ) (diffUTCTime <$> maxt <*> mint)
    case mint of
        Left _ -> return []
        Right mt -> do
            nofDays <- timediff
            return $ fmap (\x -> addUTCTime (24*60*60*( fromRational x)) mt) [0 .. (toRational nofDays)]


-- https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/containers-0.3.0.0/Data-Map.html
alignTS' :: Num a => [UTCTime] -> [(UTCTime, a)] -> [(UTCTime, Maybe a)]
alignTS' [] ts = map (second (Just))  ts  -- Add "getDateTimeInterval"
alignTS' _ [] = []
alignTS' idx ts = zip idx' combinedValues
     where   tvMap = foldl (\mm (key, value) -> Map.insert key value mm) Map.empty ts
             idx' = sort idx
             combinedValues = map (\v -> Map.lookup v tvMap) idx'                          

alignTS :: Num a => Either String [UTCTime] -> Either String [(UTCTime, a)] -> Either String [(UTCTime, Maybe a)]                    
alignTS idx ts = do 
    ind <- idx
    dta <- ts
    return $ alignTS' ind dta

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
    

-- ts <- priceTimeSeries "IBM"
-- let ts1 = fmap (take 20) ts
-- let tsIndex = fmap (filter isAWorkingDay) $ getDateTimeInterval ts1
-- let tsa = alignTS tsIndex ts1
-- let tsb = backFillTS tsa