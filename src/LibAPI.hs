{-# LANGUAGE OverloadedStrings #-}

module LibAPI
( 
    viewTL,
    downloadH,
    priceTimeSeries,
    getDateTimeInterval,
    isWorkingDay,
    alignTS
) where

import LibYahoo
import LibCSV

-- import CSV related functions
import Text.CSV
import qualified Data.ByteString.Lazy.UTF8 as DBLU
import qualified Data.ByteString.Lazy.Char8 as C

-- import other
import Data.Time
import Data.Time.Calendar.WeekDate
import qualified Data.Map as Map
import Data.Either
import Control.Arrow (first, second)

-- import graphiccs
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

-- import system process
import System.Process


preparePrices :: Num b => Either String [(UTCTime, b)] -> [(LocalTime, b)]
preparePrices x = map (first (utcToLocalTime utc)) (concat $ rights [x])

priceTimeSeries :: String -> IO (Either String [(UTCTime, Double)] )
priceTimeSeries ticker = do 
   ydata <- getYahooData ticker :: IO (Either YahooException C.ByteString)
   let ycsv = either (\_ -> Left YStatusCodeException) id
               (mapM (\x -> parseCSV "Ticker" (DBLU.toString x )) ydata)
   let dates = getColumnInCSV ycsv "Date"
   let closep = getColumnInCSV ycsv "Adj Close"
   return $ zip <$> (map (read2UTCTime "%Y-%m-%d") <$> dates) <*> (map read2Double <$> closep)

---------------------------------------------
-- Time Series
---------------------------------------------

getDateTimeInterval :: Either String [(UTCTime, Double)] -> Either String [UTCTime]
getDateTimeInterval tseries = do
    let mint = fmap (fst . minimum) tseries
    let maxt = fmap (fst . maximum) tseries
    let timediff = fmap (\x -> (toRational x) / (60*60*24) ) (diffUTCTime <$> maxt <*> mint)
    case mint of
        Left _ -> return []
        Right mt -> do
            nofDays <- timediff
            return $ fmap (\x -> addUTCTime (24*60*60*( fromRational x)) mt) [0 .. (toRational nofDays)]


isWorkingDay::UTCTime -> Bool
isWorkingDay x = 
    let myWeekDay = (toWeekDate . utctDay) x
        (_, _, aWeekDay) = myWeekDay
    in aWeekDay < 6

-- https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/containers-0.3.0.0/Data-Map.html
alignTS' :: Num a => [UTCTime] -> [(UTCTime, a)] -> [(UTCTime, Maybe a)]
alignTS' [] ts = map (second (Just))  ts
alignTS' _  [] = []
alignTS' idx ts = zip idx combinedValues
     where   tvMap = foldl (\mm (key, value) -> Map.insert key value mm) Map.empty ts
             combinedValues = map (\v -> Map.lookup v tvMap) idx                          

alignTS :: Num a => Either String [UTCTime] -> Either String [(UTCTime, a)] -> Either String [(UTCTime, Maybe a)]                    
alignTS idx ts = do 
    ind <- idx
    dta <- ts
    return $ alignTS' ind dta

-- ts <- priceTimeSeries "IBM"
-- let ts1 = fmap (take 20) ts
-- let a = fmap (filter isWorkingDay) $ getDateTimeInterval ts1
    
-- ------------------------------------------
-- API
---------------------------------------------

viewTL :: [String] -> IO ()  
viewTL [fileName] = do  
    contents <- readFile fileName  
    let tickers = lines contents  
        numberedTasks = zipWith (\n linex -> show n ++ " - " ++ linex) [0..] tickers  
    putStr $ unlines numberedTasks

downloadH :: [String] -> IO ()
downloadH [fileName, numberString] = do
    contents <- readFile fileName
    let number = read numberString :: Int
    let ticker_line = (lines contents ) !! number 
    let ticker_csv = parseCSV "Ticker" ticker_line
    case ticker_csv of
        Left _ -> return ()
        Right ticker -> do
                let tk = (concat ticker) !! 0
                ts <- priceTimeSeries tk 
                -- Plot
                let plotFileName = "plot-series.svg"
                toFile def plotFileName $ plot (line "" [preparePrices ts])
                putStrLn $ tk ++ " plot saved to: " ++ plotFileName
                createProcess (shell $ "firefox " ++ plotFileName)
                return () 

