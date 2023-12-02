import Data.List
import Data.Ord

data Plate = Plate String deriving (Show, Eq)

parsePlate :: String -> Maybe Plate
parsePlate string 
  | string == "AB" = Just (Plate string)
  | otherwise  = Nothing

data Money = Money Int
  deriving Show

renderMoney :: Money -> String
renderMoney (Money cents) = show (fromIntegral cents / 100)

(+!) :: Money -> Money -> Money
(Money a) +! (Money b) = Money (a + b)

scale :: Money -> Double -> Money
scale (Money a) x = Money (round (fromIntegral a * x))

addVat :: Money -> Money
addVat m = m +! scale m 0.24

data Person = Person {age::Int, name::String}
  deriving Show

data SortOrder = Ascending | Descending
data SortField = Name | Age

sortByField :: SortField -> [Person] -> [Person]
sortByField Name ps = sortBy (comparing name) ps
sortByField Age ps = sortBy (comparing age) ps

sortPersons :: SortField -> SortOrder -> [Person] -> [Person]
sortPersons field Ascending ps = sortByField field ps
sortPersons field Descending ps = reverse $ sortByField field ps

persons = [Person 21 "Frida", Person 88 "Hans"]

data NonEmpty a = a :| [a]

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty [] = Nothing
nonEmpty (x:xs) = Just (x :| xs)

toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs

data Sum a = Sum a
instance Num a  => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a+b)

data Product a = Product a
instance Num a => Semigroup (Product a) where
  Product a <> Product b = Product (a*b)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

instance Num a => Monoid (Product a) where
  mempty = Product 1

example'  = foldMap (\x -> (Sum x, Product x)) [1::Int, 5, 2]

data Discount = DiscountPercent Int | DiscountConstant Int | MinimumPrice Int | ForCustomer String Discount | Many [Discount]

applyDiscount :: String -> Int -> Discount -> Int
applyDiscount _ price (DiscountPercent percent) = price - (price * percent) `div` 100
applyDiscount _ price (DiscountConstant discount) = price - discount
applyDiscount _ price (MinimumPrice minPrice) = max price minPrice
applyDiscount customer price (ForCustomer target discount)
  | customer == target = applyDiscount customer price discount
  | otherwise = price
applyDiscount customer price (Many discounts) = go price discounts
  where go p [] = p
        go p (d:ds) = go (applyDiscount customer p d) ds

data Set a = Set [a]

instance Semigroup (Set [a]) where
  (Set as) <> (Set bs) = Set (as ++ bs)
