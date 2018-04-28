{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yadata.LibAPI

import System.Environment


import Text.XML.HXT.Core
import Data.String.Utils

-- http://hackage.haskell.org/package/wreq-0.5.1.0/docs/Network-Wreq-Session.html
-- https://stackoverflow.com/questions/44044263/yahoo-finance-historical-data-downloader-url-is-not-working
-- https://stackoverflow.com/questions/1317399/getting-the-local-appdata-folder-in-haskell

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("view", viewTL), ("graph", downloadH2Graph),
              ("download", downloadH2File), ("mva", movAvg), ("maStrat", movAvgStrategy) ]

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

-- https://wiki.haskell.org/HXT
-- https://wiki.haskell.org/HXT/Practical
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/tagsoup
-- https://stackoverflow.com/questions/41215314/records-from-trs-in-an-html-table-using-arrows-and-hxt-in-haskell?noredirect=1&lq=1

is x = deep (isElem >>> hasName x)
is2 x y = deep (isElem >>> hasName x <+> (isElem >>> hasName y) )

-- https://hackage.haskell.org/package/hxt-9.3.1.16/docs/Text-XML-HXT-Arrow-ReadDocument.html
parseXML file = readDocument [ withValidate no, withRemoveWS yes, withParseHTML yes, withWarnings no ] file

getTable :: ArrowXml a => a XmlTree [[String]]
getTable =  is "table" >>> listA (rows >>> listA cols) where
    rows = getChildren >>> is "tr"
    cols = getChildren >>> is2 "th" "td" /> getText

main :: IO () --(Either YahooException C.ByteString)
main = do
    -- (command:args) <- getArgs
    -- let (Just action) = lookup command dispatch
    -- action args
    a <- runX $ parseXML "./test2.html" >>> getTable
    let b = (fmap.fmap) (\x-> replace "," "" x) (concat a)
    print b

    return ()

