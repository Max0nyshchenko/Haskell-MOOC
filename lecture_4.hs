import Prelude
import Data.List
import Data.Array
import qualified Data.Map as Map

zipExample = zip [1,2,3] [True,False,True]
unzipExample = unzip [("Fred",1), ("jack", 2), ("Helen", 3)]
partitionExample = partition (>0) [-1,1,-4,3,2,0]

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

sumIf :: [(Bool, Int)] -> Int
sumIf [] = 0
sumIf ((True, x):xs) = x + sumIf xs
sumIf ((False,_):xs) = sumIf xs

sumNumbers xs = foldr (+) 0 xs

f :: (Eq a) => (a -> a) -> a -> Bool
f g x = x == g x

bothPairEqual :: (Eq a, Eq b) => a -> a -> b -> b -> Bool
bothPairEqual left1 left2 right1 right2 = left1 == left2 && right1 == right2

comparing' :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing' f x y = compare (f x) (f y)

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (comparing' length)

values = Map.fromList [("z", 3), ("w", 4)]
values1 = Map.lookup "z" values
values2 = Map.insert "x" 7 values
values3 = foldr (+) 0 values

withdraw :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw account amount bank = case Map.lookup account bank of
  Nothing -> bank
  Just sum -> Map.insert account (sum-amount) bank

withdraw' :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw' account amount bank = Map.adjust (\x -> x-amount) account bank

myArray :: Array Int String
myArray = array (7, 11) [(7, "seven"), (8, "eight"), (9, "nine"), (10, "ten"), (11, "ELEVEN")]

myArray' :: Array Int String
myArray' = listArray (7,11) ["seven", "eight", "nine", "ten", "ELEVEN"]

element'= myArray ! 8
updatedMyArray = myArray // [(8, "ocho"), (9, "nueve")]
myArrayLength = length myArray
