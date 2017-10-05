{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module LibYahoo
    (   getYahooData
      , getYahooDataSafe
      , YahooException (..)
    ) where

import Network.Wreq
import Text.Regex.PCRE
import Control.Lens
import Data.Int
import qualified Data.ByteString.Lazy as B (ByteString, drop, take, pack)
import qualified Network.Wreq.Session as S
import qualified Data.ByteString.Lazy.Char8 as C
-- Exception handling
import Control.Exception as E
import Control.Monad.Except
import Data.Typeable

crumbleLink :: String -> String
crumbleLink ticker = "https://finance.yahoo.com/quote/" ++ ticker ++ "/history?p=" ++ ticker


yahooDataLink :: String -> String -> String
yahooDataLink ticker crumb =
   "https://query1.finance.yahoo.com/v7/finance/download/" ++ ticker ++
   "?period1=1201686274&period2=1504364674&interval=1d&events=history&crumb=" ++ crumb


crumblePattern :: String
crumblePattern = "CrumbStore\":{\"crumb\":\"(.*?)\"}" :: String


getCrumble :: B.ByteString -> B.ByteString
getCrumble crumbText = do
   let test = crumbText =~ crumblePattern :: (Int, Int)
   -- let shift = 22 :: Int64
   -- let crumb = DBL.take (fromIntegral (snd test) - (shift + 2) ) (DBL.drop (fromIntegral (fst test) + shift) crumbText)
   B.take (fromIntegral (snd test) - 24) (B.drop (fromIntegral (fst test) + 22) crumbText)


getYahooData :: String -> IO B.ByteString
getYahooData ticker = S.withSession $ \sess -> do
   -- get session related data
   r <- S.get sess (crumbleLink "KO")
   let rb = r ^. responseBody
   let crb = getCrumble rb
   -- get time series
   r2 <- S.get sess (yahooDataLink ticker (C.unpack crb))
   let r2b = r2 ^. responseBody
   return r2b

------------------------------------------------------------------------------------------------------

data YahooException
  = YStatusCodeException
  | YCookieCrumbleException
  deriving (Typeable)

instance Show YahooException where
  show YStatusCodeException = "Yadata data fetch exception!"
  show YCookieCrumbleException = "Yadata cookie crumble exception!"

instance Exception YahooException

fetchCompanyData :: String -> IO (Either YahooException B.ByteString)
fetchCompanyData ticker =
  S.withSession $ \sess -> do
    r <- S.get sess (crumbleLink "KO")
    let rs = r ^. responseStatus . statusCode
    let rb = r ^. responseBody
    if rs /= 200
      then return $ Left YCookieCrumbleException
      else do
        let crb = getCrumble rb
        r2 <- S.get sess (yahooDataLink ticker (C.unpack crb))
        let r2s = r2 ^. responseStatus . statusCode
        let r2b = r2 ^. responseBody
        if r2s /= 200
          then return $ Left YStatusCodeException
          else return $ Right r2b

getCrumbleSafe :: IO (Either YahooException C.ByteString)
getCrumbleSafe =
  S.withSession $ \sess -> do
    d <-
      E.try
        (let !x = S.get sess (crumbleLink "KO")
         in x) :: IO (Either YahooException (Response C.ByteString))
    case d of
      Left e -> return $ Left YCookieCrumbleException
      Right crb -> do
        let status = crb ^. responseStatus . statusCode
        let body = crb ^. responseBody
        if status /= 200
          then return $ Left YCookieCrumbleException
          else return $ Right $ getCrumble body


getYahooDataSafe :: String -> IO String
getYahooDataSafe ticker = do
  crumb <- getCrumbleSafe :: IO (Either YahooException C.ByteString)
  case crumb of
    Left e -> return $ show YCookieCrumbleException
    Right crb ->
      S.withSession $ \sess -> do
        result <-
          E.try
            (let !x = S.get sess (yahooDataLink ticker (C.unpack crb))
             in x) :: IO (Either YahooException (Response C.ByteString))
        case result of
          Left e -> return $ show YStatusCodeException
          Right d -> do
            let status = d ^. responseStatus . statusCode
            let body = d ^. responseBody
            if status /= 200
              then return $ show YStatusCodeException
              else return $ C.unpack body




handler :: YahooException -> IO ()
handler YStatusCodeException = putStrLn "status code exception"
handler YCookieCrumbleException = putStrLn "cookie crumble exception"


testIt = tryIt `catch` handler

tryIt = do
    print "------"
    x <- getYahooDataSafe "A"
    print x
    print "======"
