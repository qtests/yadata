{-# LANGUAGE OverloadedStrings #-}

module Main where

import LibYahoo
import LibCSV

-- import CSV related functions
import Text.CSV
import qualified Data.ByteString.Lazy.UTF8 as DBLU

-- import other
import Data.Time
import Data.Either
import Control.Arrow (first)

-- import graphiccs
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

-- import system process
import System.Process
import qualified Data.ByteString.Lazy.Char8 as C
-- http://hackage.haskell.org/package/wreq-0.5.1.0/docs/Network-Wreq-Session.html
-- https://stackoverflow.com/questions/44044263/yahoo-finance-historical-data-downloader-url-is-not-working
-- https://stackoverflow.com/questions/1317399/getting-the-local-appdata-folder-in-haskell

prices :: Num b => Either String [(UTCTime, b)] -> [(LocalTime, b)]
prices x = map (first (utcToLocalTime utc)) (concat $ rights [x])

priceTimeSeries :: String -> IO (Either String [(UTCTime, Double)] )
priceTimeSeries ticker = do 
   ydata <- getYahooData ticker :: IO (Either YahooException C.ByteString)
   let ycsv = either (\_ -> Left YStatusCodeException) id
               (mapM (\x -> parseCSV "Ticker" (DBLU.toString x )) ydata)
   let dates = getColumnInCSV ycsv "Date"
   let closep = getColumnInCSV ycsv "Adj Close"
   return $ zip <$> (map (read2UTCTime "%Y-%m-%d") <$> dates) <*> (map read2Double <$> closep)
  

main :: IO (Either YahooException C.ByteString)
main = do
    
    ts <- priceTimeSeries "IBM"

    -- Plot
    let plotFileName = "plot-series.svg"
    toFile def plotFileName $ plot (line "" [prices ts])
    putStrLn $ "Plot saved to: " ++ plotFileName
    createProcess (shell $ "firefox " ++ plotFileName)
    return $ Right "__End__"
