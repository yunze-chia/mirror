module App.Options where

import Options.Applicative
  ( Parser,
    ParserInfo,
    auto,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    short,
    strOption,
    value,
    (<**>),
  )

data Option = Option
  { filePath :: String,
    baseCurrency :: String,
    portfolioValue :: Double
  }

fileOpt :: Parser String
fileOpt =
  strOption
    ( long "file"
        <> short 'f'
        <> metavar "FILEPATH"
        <> help "Path to CSV file containing all transations, required"
    )

baseOpt :: Parser String
baseOpt =
  strOption
    ( long "currency"
        <> short 'c'
        <> metavar "CURRENCY"
        <> help "Base currency for the portfolio, symbol must match that in input file, required"
    )

valueOpt :: Parser Double
valueOpt =
  option
    auto
    ( long "value"
        <> short 'v'
        <> metavar "VALUE"
        <> value 0
        <> help "Present mark-to-market value of portfolio, default: 0"
    )

optParser :: Parser Option
optParser = Option <$> fileOpt <*> baseOpt <*> valueOpt

opts :: ParserInfo Option
opts =
  info
    (optParser <**> helper)
    ( fullDesc
        <> header "Mirror - Returns calculator for an investment portfolio"
    )
