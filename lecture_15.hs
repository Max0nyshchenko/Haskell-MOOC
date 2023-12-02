import Control.Applicative
import Text.Read

data Currency = EUR | USD deriving (Show, Eq)

data Money = Money Int Currency deriving (Show, Eq)

parseCurrency :: String -> Maybe Currency
parseCurrency "e" = pure EUR
parseCurrency "E" = pure EUR
parseCurrency "$" = pure USD
parseCurrency _ = Nothing

parseAmount :: String -> Maybe Int
parseAmount = readMaybe

parseMoney :: String -> String -> Maybe Money
parseMoney amountString currencyString = liftA2 Money (parseAmount amountString) (parseCurrency currencyString)

negated = negate <$> [1, 2, 3] -- [-1,-2,-3]

combinedMultiplied = [(+ 1), (* 2)] <*> [10, 100] -- [11,101,20,200]
