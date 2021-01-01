module Main where

import App.Options (Option (..), opts)
import App.Types (Portfolio (..), Transaction (..))
import Data.ByteString.Lazy as BS (readFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Map as M (empty, insertWith)
import Data.Maybe (mapMaybe)
import Data.Time (Day, diffDays, getCurrentTime, utctDay)
import GHC.Float (int2Double)
import Options.Applicative (execParser)

insertTransaction :: Portfolio -> Transaction -> Portfolio
insertTransaction (Portfolio p) (Transaction _ crVal cr drVal dr) = Portfolio . addDebits . addCredits $ p
  where
    addCredits = M.insertWith (+) cr (- crVal)
    addDebits = M.insertWith (+) dr drVal

type Cashflow = (Int, Double) -- days, amount

readCashflows :: String -> Day -> [Transaction] -> [Cashflow]
readCashflows baseCurr today = mapMaybe extractRelevant
  where
    extractRelevant (Transaction d crVal cr drVal dr)
      | cr == baseCurr = Just (fromInteger $ diffDays today d, - crVal)
      | dr == baseCurr = Just (fromInteger $ diffDays today d, drVal)
      | otherwise = Nothing

dietz :: [Cashflow] -> Double -> (Double, Double)
dietz cashflows endingValue = (annualizedReturn, totalInflow)
  where
    -- note that credit values are negative
    maxPeriod = maximum $ 0 : [int2Double days | (days, _) <- cashflows]
    averageCapital = sum [int2Double days / maxPeriod * (- value) | (days, value) <- cashflows]
    totalInflow = sum [value | (_, value) <- cashflows]
    weightedReturn = (endingValue + totalInflow) / averageCapital
    annualizedReturn = (1 + weightedReturn) ** (365.25 / maxPeriod) - 1

main :: IO ()
main = do
  Option file currency value <- execParser opts
  contents <- BS.readFile file
  case decodeByName contents of
    Left err -> putStrLn $ "Unable to parse file: " ++ err
    Right (_, transactions) -> do
      today <- utctDay <$> getCurrentTime
      let portfolio = foldl insertTransaction (Portfolio M.empty) transactions
          cashflows = readCashflows currency today $ toList transactions
          (annualizedReturn, historicalCost) = dietz cashflows value
      print portfolio
      putStrLn $ "Portfolio value: " ++ show value
      putStrLn $ "Historical cost: " ++ show historicalCost
      putStrLn $ "Absolute profit: " ++ show (value + historicalCost)
      putStrLn $ "Annualized return: " ++ show annualizedReturn
