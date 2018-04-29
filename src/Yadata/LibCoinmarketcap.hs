{-# LANGUAGE OverloadedStrings #-}

module Yadata.LibCoinmarketcap
    ( 
      getCoinmarkData
    , getCoinmarkHistoData
    , CoinmarkException(..)    
    ) where

import Text.XML.HXT.Core
import Control.Arrow.ArrowTree
import Text.XML.HXT.Arrow.XmlArrow
import Data.Tree.NTree.TypeDefs
import Data.String.Utils

import Data.Time
-- import Data.Time.Clock.POSIX
import Data.Typeable

import qualified Data.ByteString.Lazy.Char8 as C

-- https://wiki.haskell.org/HXT
-- https://wiki.haskell.org/HXT/Practical
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/tagsoup
-- https://stackoverflow.com/questions/41215314/records-from-trs-in-an-html-table-using-arrows-and-hxt-in-haskell?noredirect=1&lq=1

is :: (ArrowTree a, ArrowXml a) => String -> a (NTree XNode) XmlTree
is x = deep (isElem >>> hasName x)

is2 :: (ArrowTree a, ArrowXml a) => String -> String -> a (NTree XNode) XmlTree
is2 x y = deep (isElem >>> hasName x <+> (isElem >>> hasName y) )

getTable :: ArrowXml a => a XmlTree [[String]]
getTable =  is "table" >>> listA (rows >>> listA cols) where
    rows = getChildren >>> is "tr"
    cols = getChildren >>> is2 "th" "td" /> getText

-- https://hackage.haskell.org/package/hxt-9.3.1.16/docs/Text-XML-HXT-Arrow-ReadDocument.html
parseXML :: String -> IOStateArrow s b XmlTree
parseXML file = readDocument [ withValidate no, withRemoveWS yes, withParseHTML yes, withWarnings no ] file

data CoinmarkException
    = CStatusCodeException
    | CWrongTickerException
    deriving (Typeable)


getCoinmarkData :: String -> IO (Either CoinmarkException [[String]])
getCoinmarkData ticker = do
    dataTable <- runX $ parseXML "./test2.html" >>> getTable
    return $ Right $ (fmap.fmap) (\x-> replace "," "" x) (concat dataTable)

    
getCoinmarkHistoData :: String -> UTCTime -> UTCTime-> IO (Either CoinmarkException C.ByteString)
getCoinmarkHistoData ticker startDate endDate = undefined






