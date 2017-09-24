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

-- http://hackage.haskell.org/package/wreq-0.5.1.0/docs/Network-Wreq-Session.html
-- https://stackoverflow.com/questions/44044263/yahoo-finance-historical-data-downloader-url-is-not-working
-- https://stackoverflow.com/questions/1317399/getting-the-local-appdata-folder-in-haskell

prices :: Num b => Either String [(UTCTime, b)] -> [(LocalTime, b)]
prices x = map (first (utcToLocalTime utc)) (concat $ rights [x])


main :: IO ()
main = do
   
   yd <- getYahooDataSafe "MU"

   let yd_csv = parseCSV "MU" (DBLU.toString yd)
   let dates = getColumnInCSV yd_csv "Date"
   let closep = getColumnInCSV yd_csv "Adj Close"  
   
   let ts = zip <$> (map (read2UTCTime "%Y-%m-%d") <$> dates) <*> (map read2Double <$> closep)

   -- Plot
   let plotFileName = "plot-series.svg"
   toFile def plotFileName $ plot (line "" [prices ts])
   putStrLn $ "Plot saved to: " ++ plotFileName
   createProcess (shell $ "firefox " ++ plotFileName)
  
   print "__End__"
