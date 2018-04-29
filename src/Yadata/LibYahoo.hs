{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Yadata.LibYahoo
  ( getYahooData
  , getYahooHistoData    
  , YahooException(..)
  ) where

import Control.Exception as E
import Control.Lens
-- import Control.Monad.Except
import qualified Data.ByteString.Lazy as B (ByteString, drop, take)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import Network.HTTP.Client
import Network.HTTP.Client.TLS
-- import Network.HTTP.Simple hiding (httpLbs)
import qualified Network.Wreq as W (responseBody, responseStatus, statusCode)
import Text.Regex.PCRE

crumbleLink :: String -> String
crumbleLink ticker =
  "https://finance.yahoo.com/quote/" ++ ticker ++ "/history?p=" ++ ticker

  -- https://stackoverflow.com/questions/12916353/how-do-i-convert-from-unixtime-to-a-date-time-in-haskell
  
yahooDataLink4TimePeriod :: (Integral a, Show a) => String -> String -> a -> a -> String
yahooDataLink4TimePeriod ticker crumb startDate endDate =
    "https://query1.finance.yahoo.com/v7/finance/download/" ++ ticker ++
    "?period1=" ++ show startDate ++"&period2=" ++ show endDate ++
    "&interval=1d&events=history&crumb=" ++ crumb

crumblePattern :: String
crumblePattern = "CrumbStore\":{\"crumb\":\"(.*?)\"}" :: String

getCrumble :: B.ByteString -> B.ByteString
getCrumble crumbText = do
  let test = crumbText =~ crumblePattern :: (Int, Int)
  B.take
    (fromIntegral (snd test) - 24)
    (B.drop (fromIntegral (fst test) + 22) crumbText)


------------------------------------------------------------------------------------------------------
data YahooException
  = YStatusCodeException
  | YCookieCrumbleException
  | YWrongTickerException
  deriving (Typeable)

instance Show YahooException where
  show YStatusCodeException    = "Yadata :: data fetch exception!"
  show YCookieCrumbleException = "Yadata :: cookie crumble exception!"
  show YWrongTickerException   = "Yadata :: wrong ticker passed in!"

instance Exception YahooException

getYahooData :: String -> IO (Either YahooException C.ByteString)
getYahooData ticker = do
  endDate <- getCurrentTime
  let starDate = UTCTime  (fromGregorian 2000 01 01) 0
  getYahooHistoData ticker starDate endDate



getYahooHistoData :: String -> UTCTime -> UTCTime-> IO (Either YahooException C.ByteString)
getYahooHistoData ticker startDate endDate= do
  manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
  setGlobalManager manager
  cookieRequest <- parseRequest (crumbleLink "KO")
  crumb <-
    E.try (httpLbs cookieRequest manager) :: IO (Either YahooException (Response C.ByteString))
  case crumb of
    Left e -> return $ Left YCookieCrumbleException
    Right crb -> do
      now <- getCurrentTime
      let (jar1, _) = updateCookieJar crb cookieRequest now (createCookieJar [])
      let body = crb ^. W.responseBody
      -- qEndDate <- getPOSIXTime
      dataRequest <-
        parseRequest (yahooDataLink4TimePeriod ticker 
                      (C.unpack $ getCrumble body) 
                      (round (utcTimeToPOSIXSeconds startDate) :: Integer)
                      (round (utcTimeToPOSIXSeconds endDate) :: Integer)
                     )
      now2 <- getCurrentTime
      let (dataReq, jar2) = insertCookiesIntoRequest dataRequest jar1 now2
      result <-
        E.try (httpLbs dataReq manager) :: IO (Either YahooException (Response C.ByteString))
      case result of
        Left e -> return $ Left YStatusCodeException
        Right d -> do
          let body2 = d ^. W.responseBody
          let status = d ^. W.responseStatus . W.statusCode
          if status == 200
            then return $ Right body2
            else return $ Left YStatusCodeException