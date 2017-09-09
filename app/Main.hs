{-# LANGUAGE OverloadedStrings #-}

module Main where

import LibYahoo
import LibCSV

-- import Other
import Text.CSV
import qualified Data.ByteString.Lazy.UTF8 as DBLU


-- http://hackage.haskell.org/package/wreq-0.5.1.0/docs/Network-Wreq-Session.html
-- https://stackoverflow.com/questions/44044263/yahoo-finance-historical-data-downloader-url-is-not-working

main :: IO ()
main = do
   
   yd <- getYahooData "MU"
   let yd_csv = parseCSV "MU" (DBLU.toString yd)
   let dates = either (\_ -> Left "Network problem!") (\x -> applyToColumnInCSV id x "Date") yd_csv 
   let closep = either (\_ -> Left "Network problem!") (\x -> applyToColumnInCSV id x "Adj Close" ) yd_csv  
   
   print $ zip <$> (map (read2UTCTime "%Y-%m-%d") <$> dates) <*> (map read2Double <$> closep)

   print "__End__"