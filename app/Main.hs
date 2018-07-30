{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yadata.LibAPI

import System.Environment


-- http://hackage.haskell.org/package/wreq-0.5.1.0/docs/Network-Wreq-Session.html
-- https://stackoverflow.com/questions/44044263/yahoo-finance-historical-data-downloader-url-is-not-working
-- https://stackoverflow.com/questions/1317399/getting-the-local-appdata-folder-in-haskell

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("view", viewTL), ("graph", downloadH2Graph), 
              ("download", downloadH2File),  ("rundownload", runDownloadH2File),
              ("mva", movAvg), ("maStrat", movAvgStrategy), ("coin", downloadCoin) ]

-- To view ticker file, run:
--    stack exec yadata-exe view sp500.csv | more

-- To download historical time series and make a graph for the company IBM:
--    stack exec yadata-exe graph IBM

-- To download historical time series and save them to a file for the companies IBM, MSFT, AAPL and KO:
--    stack exec yadata-exe download IBM MSFT AAPL KO

-- To download historical time series, calculate their moving averages and
-- save them to a file for the companies IBM, MSFT, AAPL and KO:
--    stack exec yadata-exe mva IBM MSFT AAPL KO

-- To download historical time series, and backtest "moving average crossover"
-- strategy and save results to a file for the companies IBM, MSFT, AAPL and KO:
--    stack exec yadata-exe maStrat IBM MSFT AAPL KO


main :: IO () --(Either YahooException C.ByteString)
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args


