repeatString n str = repeatHelper n str ""

repeatHelper n str result = if (n == 0)
                            then result
                            else repeatHelper (n-1) str (result++str)

fibonacci :: Integer -> Integer
fibonacci n = fibonacci' 0 1 n

fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci' a b 1 = b
fibonacci' a b n = fibonacci' b (a+b) (n-1)

describe :: Int -> String
describe n 
  | n == 2 = "Two"
  | even n = "Even"
  | n == 3 = "Three"
  | n > 100 = "Big!!"
  | otherwise = "The number " ++ show n

factorial n 
  | n<0 = -1
  | n==0 = 1
  | otherwise = n * factorial (n-1)

guessAge :: String -> Int -> String
guessAge "Griselda" age
  | age < 47 = "Too low!"
  | age > 47 = "Too high!"
  | otherwise = "Correct !"
guessAge "Hansel" age
  | age < 12 = "Too low!"
  | age > 12 = "Too high!"
  | otherwise = "Correct!"
guessAge name age = "wrong name!"

indexing = [1, 2, 3, 4, 5] !! 2

login :: String -> Maybe String
login "f4bulous!" = Just "unicorn73"
login "swordfish" = Just "megahacker"
login _ = Nothing

perhapsMultiply :: Int -> Maybe Int -> Int
perhapsMultiply i Nothing = i
perhapsMultiply i (Just j) = i*j

intOrZero :: Maybe Int -> Int
intOrZero Nothing = 0
intOrZero (Just i) = i

safeHead :: [a] -> Maybe a
safeHead xs = if null xs then Nothing else Just (head xs)

headOrZero :: [Int] -> Int
headOrZero xs = intOrZero (safeHead xs)

readInt :: String -> Either String Int
readInt "0" = Right 0
readInt "1" = Right 1
readInt s = Left ("Unsupported string: " ++ s)

iWantAString :: Either Int String -> String
iWantAString (Right str) = str
iWantAString (Left number) = show number

lectureParticipants :: [Either String Int]
lectureParticipants = [Right 10, Right 13, Left "esater vacation", Right 17, Left "lecturer was sick"]

describe' :: Integer -> String
describe' n = case n of 0 -> "Zero"
                        1 -> "one"
                        2 -> "an even prime"
                        n -> "the number " ++ show n

parseCountry :: String -> Maybe String
parseCountry "FI" = Just "Finland"
parseCountry "SE" = Just "Sweden"
parseCountry _ = Nothing

flyTo :: String -> String
flyTo countryCode = case parseCountry countryCode of Just country -> "You're flying to " ++ country
                                                     Nothing -> "You're not flying anywhere"

flyTo' :: String -> String
flyTo' countryCode = handleResult (parseCountry countryCode)
  where handleResult (Just country) = "You're flying to " ++ country
        handleResult Nothing  = "You're not flying anywhere"

sentenceType :: String -> String
sentenceType sentence = case last sentence of '.' -> "statement"
                                              '?' -> "question"
                                              '!' -> "exclamation"
                                              _   -> "not a sentence"

sentenceType' sentence = classify (last sentence)
  where classify '.' = "statement"
        classify '?' = "question"
        classify '!' = "exclamation"
        classify _   = "not a sentence"

motivate' :: String -> String
motivate' day  = case distanceToSunday day of
  6 -> "Nice week"
  5 -> "You're one day closer to weekend!"
  n -> if n > 1
       then show (n - 1) ++ " more day(s) until the weekend!"
       else "Relax"

motivate'' :: String -> String
motivate'' "Monday" = "Nice week"
motivate'' "Tuesday" = "One day closer"
motivate'' "Wednesday" = "3 more days"
motivate'' _          = "Relax"

motivate :: String -> String
motivate day
  | n == 6 = "Nice weekend"
  | n == 5 = "One day closer"
  | n > 1 = show (n - 1) ++ " more day(s) until the weekend!"
  | otherwise = "Relax! You don't need to work today!"
  where n = distanceToSunday day

area :: String -> Double -> Double
area shape x = case shape of 
  "square" -> square x
  "circle" -> pi * square x
  where square x = x*x

distanceToSunday :: String -> Int
distanceToSunday d = case d of 
  "Monday" -> 6
  "Tuesday" -> 5
  "Wednesday" -> 4
  "Thursday" -> 3
  "Friday" -> 2
  "Saturday" -> 1
  "Sunday" -> 0
  _ -> 0
