{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.DeepSeq
import Debug.Trace
import GHC.Generics (Generic)

viitenumeroCheck :: [Int] -> Bool
viitenumeroCheck allDigits = mod (checkSum + checkDigit) 10 == 0
  where
    (checkDigit : digits) = reverse allDigits
    multipliers = cycle [7, 3, 1]
    checkSum = Prelude.sum $ zipWith (*) multipliers digits

mapTailRecursive :: (a -> b) -> [a] -> [b]
mapTailRecursive f xs = go xs []
  where
    go (x : xs) res = go xs (res ++ [f x])
    go [] res = res

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n xs = if n > length xs then [] else myDrop (n -1) (tail xs)

foldl'Int :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldl'Int f z [] = z
foldl'Int f 0 (x : xs) = foldl'Int f (f 0 x) xs
foldl'Int f z (x : xs) = foldl'Int f (f z x) xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z [] = z
myFoldl f z (x : xs) =
  let z' = f z x
   in seq z' (myFoldl f z' xs)

newtype Inverted = Inverted Int deriving (Show, Eq)

instance Ord Inverted where
  compare (Inverted i) (Inverted j) = compare j i

typeKnot = let xs = 1 : 2 : xs in xs

data Room = Room String [(String, Room)]

describe :: Room -> String
describe (Room s _) = s

move :: Room -> String -> Maybe Room
move (Room _ directions) direction = lookup direction directions

world :: Room
world = meadow
  where
    meadow = Room "It's a flowery meadow next to a cliff." [("Stay", meadow), ("Enter cave", cave)]
    cave = Room "You are in a cave" [("Exit", meadow), ("Go deeper", tunnel)]
    tunnel = Room "This is a very dark tunnel" [("Go back", cave), ("Go left", pit), ("Go right", treasure)]
    pit = Room "You fall into a pit, there is no way out" []
    treasure = Room "A green light from a terminal, a termianl says <<loop>>." [("Go back", tunnel)]

play :: Room -> [String] -> [String]
play room [] = [describe room]
play room (d : ds) = case move room d of
  Nothing -> [describe room]
  Just r -> describe room : play r ds

game = play world ["Stay", "Enter cave", "Go deeper", "Go back", "Go deeper", "Go right"]

debug = trace "a" False || trace "b" True

sumEverySecond 0 = 0
sumEverySecond n = trace ("sumEverySecond " ++ show n) (n + sumEverySecond (n - 2))

add !x !y = x + y

submain :: IO ()
submain = do
  let !five = add (1 + 1) (1 + 2)
      !seven = add (1 + 2) (1 + 3)
  putStrLn $ "Five " ++ show five

badseq a b = b

add' :: Int -> Int -> Int
add' x y =
  let part1 = seq x part2
      part2 = seq y answer
      answer = x + y
   in part1

add'' :: Int -> Int -> Int
add'' x y = x `seq` y `seq` x + y

data RunningTotal = RunningTotal {sum :: Int, count :: Int}

printAverage :: RunningTotal -> IO ()
printAverage (RunningTotal sum count)
  | count == 0 = error "Need at least one value"
  | otherwise = print (fromIntegral sum / fromIntegral count :: Double)

printListAverage :: [Int] -> IO ()
printListAverage =
  go (RunningTotal 0 0)
  where
    go rt [] = printAverage rt
    go (RunningTotal !sum !count) (x : xs) = let rt = RunningTotal (sum + x) (count + 1) in go rt xs

runningTotalMain :: IO ()
runningTotalMain = printListAverage [1 .. 1000000]

instance NFData RunningTotal where
  rnf (RunningTotal sum count) = sum `deepseq` count `deepseq` ()

printListAverage' :: [Int] -> IO ()
printListAverage' = go (RunningTotal 0 0)
  where
    go rt [] = printAverage rt
    go (RunningTotal sum count) (x : xs) = let rt = RunningTotal (sum + x) (count + 1) in rt `deepseq` go rt xs

data RunningTotalStrict = RunningTotalStrict {sumStrict :: !Int, countStrict :: !Int} deriving (Generic)

data Foo = Foo Int

data Bar = Bar !Int

newtype Baz = Baz Int

mysum :: [Int] -> Int
mysum list0 = go list0 0
  where
    go [] total = total
    go (x : xs) total = go xs $! total + x

average :: [Int] -> Double
average list0 = go list0 (0, 0)
  where
    go [] (total, count) = fromIntegral total / count
    go (x : xs) (total, count) = go xs $!! (total + x, count + 1)
