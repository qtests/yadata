{-# LANGUAGE OverloadedStrings #-}

module LibYahoo
    (   getYahooData
      , getYahooDataSafe
    ) where

import Network.Wreq
import Text.Regex.PCRE
import Control.Lens
import Data.Int
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.UTF8 as DBLU
import qualified Network.Wreq.Session as S
import Data.Time.Clock
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple as NS
import Data.Maybe (fromMaybe)
import Data.Time
-- Exception handling
import Control.Exception as E


data YahooData = YahooData
  { yahooDataDate :: L8.ByteString
  , yahooDataOpen :: L8.ByteString
  , yahooDataHigh :: L8.ByteString
  , yahooDataLow :: L8.ByteString
  , yahooDataClose :: L8.ByteString
  , yahooDataAdjClose :: L8.ByteString
  , yahooDataVolume :: L8.ByteString
  } deriving (Show, Eq)


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

-- https://stackoverflow.com/questions/5631116/arising-from-a-use-of-control-exception-catch

getYahooDataSafe :: String -> IO DBL.ByteString
getYahooDataSafe ticker = do
   dataDownload <- E.try $ (getYahooData ticker) :: IO (Either E.SomeException DBLU.ByteString)
   case dataDownload of
        Left  e        -> return $ DBLU.fromString $ show e  -- DBLU.fromString []
        Right response -> return response



------------------------------------------------------------------------------------------------------

getYahooResponse :: String -> IO (Maybe (Response L8.ByteString))
getYahooResponse ticker = do
  cookieRequest <- NS.parseRequest $ crumbleLink ticker
  eresponse <- try $ NS.httpLBS cookieRequest :: IO (Either HttpException (Response L8.ByteString))
  case eresponse of
    Left e -> return Nothing
    Right response -> return $ Just response

getYList :: Maybe (Response L8.ByteString) -> [L8.ByteString]
getYList response = do
    case response of
      Nothing -> []
      Just a ->  L8.lines $ getResponseBody a

convertToDoubleList :: [L8.ByteString] -> [[L8.ByteString]]
convertToDoubleList blist = map (L8.split ',') blist

getYDataList :: [[L8.ByteString]] -> [YahooData]
getYDataList dl = map createRecData dl

parseBStrTime :: L8.ByteString -> Maybe UTCTime
parseBStrTime a = undefined

createRecData :: [L8.ByteString] -> YahooData
createRecData l =
 YahooData
  { yahooDataDate = l !! 0
  , yahooDataOpen = l !! 1
  , yahooDataHigh = l !! 2
  , yahooDataLow  = l !! 3
  , yahooDataClose = l !! 4
  , yahooDataAdjClose = l !! 5
  , yahooDataVolume = l !!  6
  }

getYHistorical :: String -> IO [YahooData]
getYHistorical ticker = do
    a <- getYahooResponse ticker
    return $ getYDataList . convertToDoubleList . getYList $ a
