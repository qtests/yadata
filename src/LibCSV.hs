{-# LANGUAGE OverloadedStrings #-}

module LibCSV
    ( someFun
    ) where

import Text.CSV
import Data.List

someFun :: IO ()
someFun = print "Hello World!"

{-|
   Gets a column from a CSV value.
   Returns (Left errorMessage) or (Right index)
-}
getColumnInCSV :: CSV -> String -> Either String Integer
getColumnInCSV csv column =
      case lookupResponse of
          Nothing -> Left "The column does not exist in this CSV file."
          Just x -> Right (fromIntegral x)
  where
      -- This line does the lookup to see if column is in our CSV
      lookupResponse = findIndex (== column) (head csv)
