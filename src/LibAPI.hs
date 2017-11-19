{-# LANGUAGE OverloadedStrings #-}

module LibAPI
( 
    viewTL,
    downloadH2Graph,
    priceTimeSeries,
    downloadH2File
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
downloadH2Graph [fileName, numberString] = do
    contents <- readFile fileName
    let number = read numberString :: Int
    let ticker_line = (lines contents ) !! number 
    let ticker_csv = parseCSV "Ticker" ticker_line
    case ticker_csv of
        Left _ -> return ()
        Right ticker -> do
                let tk = (concat ticker) !! 0
                ts <- priceTimeSeries tk 
                -- Plot
                let plotFileName = "plot-series.svg"
                toFile def plotFileName $ plot (line "" [preparePrices ts])
                putStrLn $ tk ++ " plot saved to: " ++ plotFileName
                createProcess (shell $ "firefox " ++ plotFileName)
                return () 

-- https://stackoverflow.com/questions/17719620/while-loop-in-haskell-with-a-condition
-- http://www.programming-idioms.org/idiom/78/do-while-loop/979/haskell
-- https://stackoverflow.com/questions/27857541/abstraction-for-monadic-recursion-with-unless

downloadH2File :: [String] -> IO ()
downloadH2File tickers = do
    let allData = do
            let startXTS = createXTSRaw [] [] []
            let tk = tickers !! 0


            ts <- priceTimeSeries tk
            let allD = combineXTSnTS startXTS tk (createTSEither ts) 

            
            return $ allD
    return ()
       
           
-- downloadH2File ["IBM", "MSFT", "APPL" ]