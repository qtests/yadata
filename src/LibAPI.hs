{-# LANGUAGE OverloadedStrings #-}

module LibAPI
( 
    viewTL,
    downloadH2Graph,
    priceTimeSeries,
    downloadH2File,
    movAvg,
    movAvgStrategy,
    plotXTS
) where

import LibYahoo
import LibCSV
import LibTS

-- import CSV related functions
import Text.CSV
import qualified Data.ByteString.Lazy.UTF8 as DBLU
import qualified Data.ByteString.Lazy.Char8 as C

-- import other
import Data.Time
import Data.Either
import Control.Arrow (first)

-- import graphiccs
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

-- import system process
import System.Process


-- #############################################################################

preparePrices :: Num b => Either String [(UTCTime, b)] -> [(LocalTime, b)]
preparePrices x = map (first (utcToLocalTime utc)) (concat $ rights [x])

priceTimeSeries :: String -> IO (Either String [(UTCTime, Double)] )
priceTimeSeries ticker = do 
   ydata <- getYahooData ticker :: IO (Either YahooException C.ByteString)
   let ycsv = either (\_ -> Left YStatusCodeException) id
               (mapM (\x -> parseCSV "Ticker" (DBLU.toString x )) ydata)
   let dates = getColumnInCSVEither ycsv "Date"
   let closep = getColumnInCSVEither ycsv "Adj Close"
   return $ zip <$> (map (read2UTCTime "%Y-%m-%d") <$> dates) <*> (map read2Double <$> closep)


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
    print $ takeXTS 2 xts
    writeFileXTS "testFile_hd.csv" xts
    return ()
          
-- downloadH2File ["IBM", "MSFT", "AAPL", "KO" ]

movAvg :: [String] -> IO ()
movAvg inInfo = do
    print inInfo
    xts <- downData inInfo (createXTSRaw [] [] [])
    print $ takeXTS 2 xts
    let result = movingAverageXTS 10 xts
    writeFileXTS "testFile_ma.csv" result
    return ()

-- stack exec yadata-exe mva "IBM" "MSFT" "AAPL" "KO"

movAvgStrategy :: [String] -> IO ()
movAvgStrategy inInfo = do
    print inInfo

    -- Data
    xts <- downData inInfo (createXTSRaw [] [] [])
    print $ takeXTS 2 xts
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
    plotXTS      "testFile_strat_plot.svg"    $ XTS indx perf conames
    
    -- launch firefox
    createProcess (shell $ "firefox testFile_strat_plot.svg")

    return ()

--    stack exec yadata-exe maStrat IBM MSFT AAPL KO

plotXTS :: (Num a, PlotValue a)=> String -> XTS a -> IO ()
plotXTS plotFileName (XTS xindex xdata xcolNames) = do
    let xin = fmap (utcToLocalTime utc) xindex
    let prepData = fmap (\x-> zip xin x ) xdata
    toFile def plotFileName $ do
        plot (line (xcolNames !! 0) [ prepData !! 0])
        plot (line (xcolNames !! 1) [ prepData !! 1])
        plot (line (xcolNames !! 2) [ prepData !! 2])
        plot (line (xcolNames !! 3) [ prepData !! 3])
    
    -- createProcess (shell $ "firefox " ++ plotFileName)
    return () 