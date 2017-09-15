{-# LANGUAGE OverloadedStrings #-}

module LibYahoo
    ( getYahooData
    ) where

import Network.Wreq
import Text.Regex.PCRE
import Control.Lens
import Data.Int
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.UTF8 as DBLU
import qualified Network.Wreq.Session as S


crumbleLink :: String -> String
crumbleLink ticker = "https://finance.yahoo.com/quote/" ++ ticker ++ "/history?p=" ++ ticker


yahooDataLink :: String -> String -> String
yahooDataLink ticker crumb = 
   "https://query1.finance.yahoo.com/v7/finance/download/" ++ ticker ++ 
   "?period1=1201686274&period2=1504364674&interval=1d&events=history&crumb=" ++ crumb 
 

crumblePattern :: String 
crumblePattern = "CrumbStore\":{\"crumb\":\"(.*?)\"}" :: String


getCrumble :: DBL.ByteString -> DBL.ByteString
getCrumble crumbText = do
   let test = crumbText =~ crumblePattern :: (Int, Int)
   -- let shift = 22 :: Int64
   -- let crumb = DBL.take (fromIntegral (snd test) - (shift + 2) ) (DBL.drop (fromIntegral (fst test) + shift) crumbText)
   DBL.take (fromIntegral (snd test) - 24) (DBL.drop (fromIntegral (fst test) + 22) crumbText)


getYahooData :: String -> IO DBL.ByteString
getYahooData ticker = S.withSession $ \sess -> do

   -- get session related data	
   r <- S.get sess (crumbleLink "KO")
   let rb = r ^. responseBody
   let crb = getCrumble rb

   -- get time series
   r2 <- S.get sess (yahooDataLink ticker (DBLU.toString crb) )
   let r2b = r2 ^. responseBody
   return r2b
