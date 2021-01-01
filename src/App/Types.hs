{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Types where

import Data.Csv (FromNamedRecord, parseNamedRecord, (.:))
import Data.List (intercalate)
import Data.Map as M (Map, filter, toList)
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)

data Transaction = Transaction
  { date :: Day,
    creditValue :: Double,
    creditAsset :: String,
    debitValue :: Double,
    debitAsset :: String
  }

instance FromNamedRecord Transaction where
  parseNamedRecord m =
    Transaction
      <$> (m .: "date" >>= iso8601ParseM)
      <*> m .: "creditValue"
      <*> m .: "creditAsset"
      <*> m .: "debitValue"
      <*> m .: "debitAsset"

newtype Portfolio = Portfolio (M.Map String Double)

instance Show Portfolio where
  show (Portfolio p) = intercalate "\n    " $ "Portfolio components:" : map displayRecord withoutZeros
    where
      withoutZeros = M.toList $ M.filter (/= 0) p
      displayRecord (symbol, value) = symbol ++ " " ++ show value
