# Mirror

Our application calculates historical money-weighted investment returns. We estimate returns using the modified Dietz method. The resulting number is similar to the modified internal rate of return (MIRR) metric.

## Usage
Our application is CLI based and takes in a CSV file as input, along with some required flags.

### Flags
```
-f,--file FILEPATH       Path to CSV file containing all transations, required
-c,--currency CURRENCY   Base currency for the portfolio, symbol must match
                          that in input file, required
-v,--value VALUE         Present mark-to-market value of portfolio, default: 0
-h,--help                Show this help text
```
Thus, an invocation would look like this.
```
mirror -f transactions.csv -c SGD -v 1000
```

### Input CSV
The CSV file consists of records for all transactions since the inception of the portfolio. We use double entry accounting to track our accounts. In simplified terms, every transaction has two components where credits represents outflows and debits represents inflows. We will illustrate with an annotated example where our base currency is in SGD.

```
# transactions.csv

date,creditValue,creditAsset,debitValue,debitAsset  -- headers are required
2020-03-01,1500,SGD,1000,USD                        -- (1)
2020-03-01,1000,USD,5,AAPL                          -- (2)
2020-06-01,0,DIV-AAPL,150,USD                       -- (3)
2020-09-01,0,DIV-AAPL,20,AAPL                       -- (4)
2020-12-01,10,AAPL,1500,USD                         -- (5)
```
Transaction details
1. conversion of SGD1500 to USD1000
2. purchase of 5 AAPL shares at USD200 each
3. AAPL issues us USD150 in dividends for our 5 shares
4. AAPL does a 5-for-1 stock split, essentially a share dividend instead of a cash dividend
5. we sell 10 AAPL shares for 1500USD

In our example we use `DIV-*` and `0` values to indicate dividends. Since the sum of all `DIV-*` values are zero, they will not appear in our output. Accordingly, assets that are bought and then fully sold results in a zero ending sum and will also be absent from our output.

### Executable
Now, assuming our remaining AAPL shares and USD are altogether worth SGD2000, we can run Mirror like this.
```
mirror --file=transactions.csv --currency=SGD --value=2000
```
Mirror will calculate and print the below metrics. Note that as timing matters for our return calculation, Mirror uses the current date and the dates given in the input.
```
Portfolio components:
    AAPL 15.0
    SGD -1500.0
    USD 1650.0
Portfolio value: 2000.0
Historical cost: -1500.0
Absolute profit: 500.0
Annualized return: 0.4097117048421537
```

## Build from source and install
Run `cabal build` in the repository root to build and `cabal install` to install the executable. `cabal` can be obtained using [ghcup](https://www.haskell.org/ghcup/) or the [`haskell` docker image](https://hub.docker.com/_/haskell).
