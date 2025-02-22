module Main where

import App.Options (Option (..), opts)
import Data.ByteString.Lazy as BS (readFile)
import Data.Csv (decodeByName)
import Data.Time (getCurrentTime, utctDay)
import Lib.Portfolio (emptyPortfolio, insertTransaction, moneyWeightedReturn, totalInflow)
import Options.Applicative (execParser)

main :: IO ()
main = do
  Option file currency value <- execParser opts
  contents <- BS.readFile file
  case decodeByName contents of
    Left err -> putStrLn $ "Unable to parse file: " ++ err
    Right (_, transactions) -> do
      today <- utctDay <$> getCurrentTime
      let initialPortfolio = emptyPortfolio currency
          portfolio = foldl insertTransaction initialPortfolio transactions
          annualizedReturn = moneyWeightedReturn portfolio today value
          historicalCost = totalInflow portfolio
      print portfolio
      putStrLn $ "Value: " ++ show value
      putStrLn $ "Historical cost: " ++ show historicalCost
      putStrLn $ "Absolute profit: " ++ show (value + historicalCost)
      putStrLn $ "Annualized return: " ++ show annualizedReturn
