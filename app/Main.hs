{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

-- import Other
import Text.CSV
import qualified Data.ByteString.Lazy.UTF8 as DBLU


-- http://hackage.haskell.org/package/wreq-0.5.1.0/docs/Network-Wreq-Session.html
-- https://stackoverflow.com/questions/44044263/yahoo-finance-historical-data-downloader-url-is-not-working

main :: IO ()
main = do
   
   yd <- getYahooData "MU"
   print $ parseCSV "MU" (DBLU.toString yd)

   print "__End__"