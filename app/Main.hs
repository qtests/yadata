{-# LANGUAGE OverloadedStrings #-}

module Main where

import LibAPI

import System.Environment

-- http://hackage.haskell.org/package/wreq-0.5.1.0/docs/Network-Wreq-Session.html
-- https://stackoverflow.com/questions/44044263/yahoo-finance-historical-data-downloader-url-is-not-working
-- https://stackoverflow.com/questions/1317399/getting-the-local-appdata-folder-in-haskell

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("view", viewTL), ("download", downloadH) ]

-- To view ticker file, run:
--    stack exec yadata-exe view sp500.csv | more

-- To download historical time series for the company XYZ in line 5, run:
--    stack exec yadata-exe downlaod sp500.csv 5

main :: IO () --(Either YahooException C.ByteString)
main = do
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args 

