{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

-- import Other


-- http://hackage.haskell.org/package/wreq-0.5.1.0/docs/Network-Wreq-Session.html
-- https://stackoverflow.com/questions/44044263/yahoo-finance-historical-data-downloader-url-is-not-working

main :: IO ()
main = do
   
   yd <- getYahooData "IBM"
   print $ yd

   print "__End__"