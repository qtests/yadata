{-# LANGUAGE OverloadedStrings #-}

module Yadata.LibAPI
(
    viewTL,
    downloadH2Graph,
    priceTimeSeries,
    downloadH2File,
    runDownloadH2File,
    movAvg,
    movAvgStrategy,
    createGraphForNewsletter,
    downloadCoin
) where

import Yadata.LibCSV
import Yadata.LibTS
import Yadata.LibYahoo
import Yadata.LibCoinmarketcap

-- import CSV related functions
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.UTF8 as DBLU
import Text.CSV

-- import other
import Control.Arrow (first)
import Data.Either
import Data.Maybe
import Data.Time
import Data.List

-- import graphics
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Backend.Diagrams hiding (toFile)
import Graphics.Rendering.Chart.Easy

-- import system process
-- import Control.Exception as E
import System.IO (FilePath)
import System.Process

import Control.Monad

-- #############################################################################

preparePrices :: Num b => Either String [(UTCTime, b)] -> [(LocalTime, b)]
preparePrices x = map (first (utcToLocalTime utc)) (concat $ rights [x])


priceTimeSeries :: String -> IO (Either String [(UTCTime, Double)] )
priceTimeSeries ticker = priceTSWithSource "yahoo" ticker


transformData :: Either String [String] -> String -> Either String [String] -> Either String [(UTCTime, Double)]
transformData (Left a) _ _ = Left a
transformData _ _ (Left a) = Left a
transformData (Right []) _ _ = Right []
transformData _ _ (Right []) = Right []
transformData indexes dateFormat values = do 
    ids <- indexes
    vals <- values
    let dates = readClean2UTCTime dateFormat ids
    let numbers = fmap read2DoubleMaybe vals
    let nidx = findIndices isNothing numbers
    return $ zip (removeAtIndexList nidx dates) (catMaybes numbers ) 
                 

priceTSWithSource :: String -> String -> IO (Either String [(UTCTime, Double)] )
priceTSWithSource source ticker
   | source == "yahoo" = do ydata <- getYahooData ticker :: IO (Either YahooException C.ByteString)
                            let dcsv = either (\_ -> Left YStatusCodeException) id
                                     (mapM (\x -> parseCSV "Ticker" (DBLU.toString x )) ydata)
                            let dates = getColumnInCSVEither dcsv "Date"
                            let closep = getColumnInCSVEither dcsv "Adj Close"
                            return $ transformData dates "%Y-%m-%d" closep

   | source == "LibCoinmarketcap" =  do dcsv <- getCoinmarkData ticker
                                        let dates = getColumnInCSVEither dcsv "Date"
                                        let closep = getColumnInCSVEither dcsv "Close"
                                        return $ transformData dates "%b %d %Y" closep

   | otherwise                    =     return $ Left "priceTSWithSource: Unknown source!"
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
                let plotFileName = "plot-series.png"
                toFile def plotFileName $ plot (line "" [preparePrices ts])
                putStrLn $ tk ++ " plot saved to: " ++ plotFileName
                createProcess (shell $ "firefox " ++ plotFileName)
                return ()

downloadCoin :: [String] -> IO ()
downloadCoin tickers = do
    let tk = tickers !! 0
    ts <- priceTSWithSource "LibCoinmarketcap" tk
    -- Plot
    let plotFileName = "plot-series.png"
    toFile def plotFileName $ plot (line "" [preparePrices ts])
    putStrLn $ tk ++ " plot saved to: " ++ plotFileName
    createProcess (shell $ "firefox " ++ plotFileName)
    return ()
-- https://stackoverflow.com/questions/17719620/while-loop-in-haskell-with-a-condition
-- https://stackoverflow.com/questions/27857541/abstraction-for-monadic-recursion-with-unless

-- Download data: a helper function

downDataExt :: [String] -> [String] -> XTS Double -> IO (XTS Double, [String])
downDataExt [] tf accum = return (accum, tf)
downDataExt (tk:rest) tkFailed accum = do
    
    print $ "Donwloading : " ++ tk

    ts <- priceTimeSeries tk
    ts <- if (isLeft ts || (fmap length ts) == Right 0)
                then priceTimeSeries tk
                else return ts

    let tkf = if (isLeft ts || (fmap length ts) == Right 0)
                    then [tk]
                    else []

    let allD = combineXTSnTS accum tk (createTSEither ts)
    if (rest == []) then return (allD, tkFailed ++ tkf)
                    else downDataExt rest (tkFailed ++ tkf) allD

downData :: [String] -> XTS Double -> IO (XTS Double)
downData [] accum = return accum
downData (tk:rest) accum = do
    print $ "Donwloading : " ++ tk
    ts <- priceTimeSeries tk
    ts <- if (isLeft ts || (fmap length ts) == Right 0)
                then priceTimeSeries tk
                else return ts
    let allD = combineXTSnTS accum tk (createTSEither ts)
    if (rest == []) then return allD
                    else downData rest allD


-- let fileName = "sp500.csv", let colName = "Ticker"
runDownloadH2File :: [String] -> IO ()
runDownloadH2File [] = return ()
runDownloadH2File [fileName,  colName]  = do
    contents <- readFile fileName
    let txt = parseCSV fileName contents
    let tickers = concat $ rights [getColumnInCSVEither txt colName]
    -- downloadH2File $ take 10 tickers 
    downloadH2File tickers 
    return ()

    
downloadH2File :: [String] -> IO ()
downloadH2File [] = return ()
downloadH2File tickers = do
    print tickers
    (xts, tks) <- downDataExt tickers [] (createXTSRaw [] [] [])
    (xts, tks) <- if (length tks > 0)
                        then downDataExt tks [] xts
                        else return (xts, tks)
    print $ takeRowXTS 2 xts
    print tks
    writeFileXTS "testFile_hd.csv" xts
    writeFile "testFile_hd_errors.csv" $ show tks
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
