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

data YahooException = YStatusCodeException | YCookieCrumbleException deriving Typeable

instance Show YahooException where
    show YStatusCodeException = "Yadata data fetch exception!"
    show YCookieCrumbleException = "Yadata cookie crumble exception!"

instance Exception YahooException

getCrumbleSafe :: IO (Either YahooException C.ByteString)
getCrumbleSafe =  do
   !d <- (E.try $ get (crumbleLink "KO")) :: IO (Either YahooException (Response C.ByteString))
   case d of
       Left e -> throw YCookieCrumbleException
       Right crb -> do
         let status = crb ^. responseStatus . statusCode
         let body = crb ^. responseBody
         if status /= 200 then
            return $ Left YCookieCrumbleException
         else
            return $ Right $ getCrumble body


getYahooDataSafe :: String -> IO (Either YahooException C.ByteString)
getYahooDataSafe ticker = do
   crumb <- getCrumbleSafe :: IO (Either YahooException  C.ByteString)
   case crumb of
       Left e -> throw YCookieCrumbleException
       Right crb -> do
         !result <- (E.try $ get (yahooDataLink ticker (C.unpack crb))) :: IO (Either YahooException (Response C.ByteString))
         case result of
           Left e -> throw YStatusCodeException
           Right d -> do
             let status = d ^. responseStatus . statusCode
             print status
             let body = d ^. responseBody
             if status /= 200 then
               return $ throw YStatusCodeException
             else
               return $ Right body
