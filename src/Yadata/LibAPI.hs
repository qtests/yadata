{-# LANGUAGE OverloadedStrings #-}

module Yadata.LibAPI
(
    viewTL,
    downloadH2Graph,
    priceTimeSeries,
    downloadH2File,
    movAvg,
    movAvgStrategy,
    createGraphForNewsletter
) where

import Yadata.LibCSV
import Yadata.LibTS
import Yadata.LibYahoo

-- import CSV related functions
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.UTF8 as DBLU
import Text.CSV

-- import other
import Control.Arrow (first)
import Data.Either
import Data.Maybe
import Data.Time

-- import graphics
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Backend.Diagrams hiding (toFile)
import Graphics.Rendering.Chart.Easy

-- import system process
import Control.Exception as E
import System.IO (FilePath)
import System.Process

import Control.Monad

-- #############################################################################

preparePrices :: Num b => Either String [(UTCTime, b)] -> [(LocalTime, b)]
preparePrices x = map (first (utcToLocalTime utc)) (concat $ rights [x])

priceTimeSeries :: String -> IO (Either String [(UTCTime, Double)] )
priceTimeSeries ticker = do
   ydata <- getYahooData ticker :: IO (Either YahooException C.ByteString)
   let ycsv = either (\_ -> Left YStatusCodeException) id
               (mapM (\x -> parseCSV "Ticker" (DBLU.toString x )) ydata)
   let dates_ = (fmap . fmap) (read2UTCTimeMaybe "%Y-%m-%d") $ getColumnInCSVEither ycsv "Date"
   let dates = fmap (\x-> if any (== Nothing) x then [] else catMaybes x) dates_
   let closep = getColumnInCSVEither ycsv "Adj Close"
   -- return $ zip <$> ( map (read2UTCTime "%Y-%m-%d") <$> dates) <*> (map read2Double <$> closep)
   return $ zip <$> ( dates ) <*> (map read2Double <$> closep)

-- ------------------------------------------
-- API
---------------------------------------------

viewTL :: [String] -> IO ()
viewTL [fileName] = do
    contents <- readFile fileName
    let tickers = lines contents
        numberedTasks = zipWith (\n linex -> show n ++ " - " ++ linex) [0..] tickers
    putStr $ unlines numberedTasks

downloadH2Graph :: [String] -> IO ()
downloadH2Graph tickers = do
    -- contents <- readFile fileName
    -- let number = read numberString :: Int
    -- let ticker_line = (lines contents ) !! number
    -- let ticker_csv = parseCSV "Ticker" ticker_line
    -- case ticker_csv of
    --     Left _ -> return ()
    --     Right ticker -> do
                -- let tk = (concat ticker) !! 0
                let tk = tickers !! 0
                ts <- priceTimeSeries tk
                -- Plot
                let plotFileName = "plot-series.svg"
                toFile def plotFileName $ plot (line "" [preparePrices ts])
                putStrLn $ tk ++ " plot saved to: " ++ plotFileName
                createProcess (shell $ "firefox " ++ plotFileName)
                return ()

-- https://stackoverflow.com/questions/17719620/while-loop-in-haskell-with-a-condition
-- https://stackoverflow.com/questions/27857541/abstraction-for-monadic-recursion-with-unless

-- Download data: a helper function
downData :: [String] -> XTS Double -> IO (XTS Double)
downData [] accum = return accum
downData (tk:rest) accum = do
    ts <- priceTimeSeries tk
    ts <- if (isLeft ts || (fmap length ts) == Right 0)
                then priceTimeSeries tk
                else return ts
    let allD = combineXTSnTS accum tk (createTSEither ts)
    if (rest == []) then return allD
                    else downData rest allD


downloadH2File :: [String] -> IO ()
downloadH2File tickers = do
    print tickers
    xts <- downData tickers (createXTSRaw [] [] [])
    print $ takeRowXTS 2 xts
    writeFileXTS "testFile_hd.csv" xts
    return ()

-- downloadH2File ["IBM", "MSFT", "AAPL", "KO" ]

movAvg :: [String] -> IO ()
movAvg inInfo = do
    print inInfo
    xts <- downData inInfo (createXTSRaw [] [] [])
    print $ takeRowXTS 2 xts
    let result = movingAverageXTS 10 xts
    writeFileXTS "testFile_ma.csv" result
    return ()

-- stack exec yadata-exe mva "IBM" "MSFT" "AAPL" "KO"

movAvgStrategy :: [String] -> IO ()
movAvgStrategy inInfo = do
    print inInfo

    -- Data
    xts <- downData inInfo (createXTSRaw [] [] [])
    print $ takeRowXTS 2 xts
    let (XTS indx prices conames) = xts

    -- Signal
    let maLong = movingAverageXTS 250 xts
    let maShort = movingAverageXTS 20 xts
    let s1 = (zipWith . zipWith) (>) prices (fst $ dataXTS maLong)
    let s2 = (zipWith . zipWith) (>) prices (fst $ dataXTS maShort)
    let sig = (fmap . fmap) (\x -> if x then 1.0 else 0.0) $
                                                 (zipWith . zipWith) (&&) s2 s1

    -- Performance
    let (XTS _ diffx _)  = logdiffXTS xts
    let perf =  fmap (scanl1 (+)) $ (zipWith . zipWith) (*) diffx sig

    -- Out
    writeFileXTS "testFile_strat_weights.csv" $ XTS indx sig conames
    writeFileXTS "testFile_strat_perform.csv" $ XTS indx perf conames
    plotXTS      "testFile_strat_plot.png"    $ takeColXTS 5 $ XTS indx perf conames

    -- launch firefox
    -- createProcess (shell $ "firefox testFile_strat_plot.svg")

    return ()


--    stack exec yadata-exe maStrat IBM MSFT AAPL KO

plotXTS :: (Num a, PlotValue a)=> String -> XTS a -> IO ()
plotXTS plotFileName (XTS xindex xdata xcolNames) = do
    let xin = fmap (utcToLocalTime utc) xindex
    let prepData = fmap (\x-> zip xin x ) xdata
    toFile def plotFileName $ do
        forM_ (zip xcolNames prepData) $ \(cname, dta) -> do
            plot (line cname [ dta ])
    return ()

-- | FilePath was testFile_strat_plot.svg
createGraphForNewsletter :: [String] -> FilePath -> IO ()
createGraphForNewsletter companyList filepath = do
    xts <- downData companyList (createXTSRaw [] [] [])
    let (XTS indx prices conames) = xts
    let maLong = movingAverageXTS 250 xts
    let maShort = movingAverageXTS 20 xts
    let s1 = (zipWith . zipWith) (>) prices (fst $ dataXTS maLong)
    let s2 = (zipWith . zipWith) (>) prices (fst $ dataXTS maShort)
    let sig = (fmap . fmap) (\x -> if x then 1.0 else 0.0) $
         (zipWith . zipWith) (&&) s2 s1
    let (XTS _ diffx _)  = logdiffXTS xts
    let perf =  fmap (scanl1 (+)) $ (zipWith . zipWith) (*) diffx sig
    plotXTS filepath $ takeColXTS 5 $ XTS indx perf conames

    return ()
