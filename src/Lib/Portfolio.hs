module Lib.Portfolio
  ( emptyPortfolio,
    insertTransaction,
    moneyWeightedReturn,
    totalInflow,
  )
where

import Data.Map as M (Map, empty, insertWith, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time (Day, diffDays)
import GHC.Float (int2Double)
import Lib.Types (Portfolio (..), Symbol, Transaction (..))

-- Portfolio helper functions --
emptyPortfolio :: Symbol -> Portfolio
emptyPortfolio c =
  Portfolio {transactions = [], baseCurrency = c, holdings = M.empty}

insertTransaction :: Portfolio -> Transaction -> Portfolio
insertTransaction p t =
  p {transactions = newTransactions, holdings = newHoldings}
  where
    newTransactions = transactions p <> [t]

    newHoldings = updateHoldings (holdings p) t

    updateHoldings :: M.Map Symbol Double -> Transaction -> M.Map Symbol Double
    updateHoldings h (Transaction _ crVal cr drVal dr) =
      addDebits . addCredits $ h
      where
        addCredits = M.insertWith (+) cr (-crVal)

        addDebits = M.insertWith (+) dr drVal

-- Portfolio summaries --
moneyWeightedReturn :: Portfolio -> Day -> Double -> Double
moneyWeightedReturn p today endingValue = dietz endingValue cashflows
  where
    cashflows = transactionsToCashFlows (baseCurrency p) today (transactions p)

totalInflow :: Portfolio -> Double
totalInflow p = fromMaybe 0 $ M.lookup (baseCurrency p) (holdings p)

-- Internal calculations --

type Cashflow = (Int, Double) -- days, amount

dietz :: Double -> [Cashflow] -> Double
dietz endingValue cashflows = annualizedReturn
  where
    -- note that credit values are negative
    maxPeriod = maximum $ 0 : [int2Double days | (days, _) <- cashflows]

    averageCapital =
      sum
        [int2Double days / maxPeriod * (-value) | (days, value) <- cashflows]

    inflow = sum [value | (_, value) <- cashflows]

    weightedReturn = (endingValue + inflow) / averageCapital

    annualizedReturn = (1 + weightedReturn) ** (365.25 / maxPeriod) - 1

transactionsToCashFlows :: String -> Day -> [Transaction] -> [Cashflow]
transactionsToCashFlows baseCurr today = mapMaybe extractRelevant
  where
    extractRelevant (Transaction d crVal cr drVal dr)
      | cr == baseCurr = Just (fromInteger $ diffDays today d, -crVal)
      | dr == baseCurr = Just (fromInteger $ diffDays today d, drVal)
      | otherwise = Nothing
