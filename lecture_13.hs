import Data.List
import qualified Data.Map as Map

login :: String -> Maybe String
login "f4sdkjf" = Just "unicr"
login "swords" = Just "megaha"
login _ = Nothing

secret :: String -> Maybe String
secret "megaha" = Just "I like roses"
secret _ = Nothing

stealSecret :: String -> Maybe String
stealSecret password =
  case login password of
    Nothing -> Nothing
    Just user -> case secret user of
      Nothing -> Nothing
      Just s -> Just ("Stole secret : " ++ s)

increase :: (Eq a) => a -> Int -> [(a, Int)] -> Maybe ([(a, Int)])
increase key val assocs =
  case Prelude.lookup key assocs of
    Nothing -> Nothing
    Just x ->
      if (val < x)
        then Nothing
        else Just ((key, val) : delete (key, x) assocs)

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing
Just x ?> f = f x

stealSecret' :: String -> Maybe String
stealSecret' password =
  login password
    ?> secret
    ?> decorate
  where
    decorate a = Just ("Stole secret " ++ a)

increase' :: Eq a => a -> Int -> [(a, Int)] -> Maybe [(a, Int)]
increase' key val assocs =
  lookup key assocs
    ?> check
    ?> buildResult
  where
    check x
      | val < x = Nothing
      | otherwise = Just x
    buildResult x = Just ((key, val) : delete (key, x) assocs)

safeHead' :: [a] -> Maybe a
safeHead' [] = Nothing
safeHead' (x : xs) = Just x

safeTail' :: [a] -> Maybe [a]
safeTail' [] = Nothing
safeTail' (x : xs) = Just xs

safeThird :: [a] -> Maybe a
safeThird xs = safeTail' xs ?> safeTail' ?> safeHead'

safeNth :: Int -> [a] -> Maybe a
safeNth 0 xs = safeHead' xs
safeNth n xs = safeTail' xs ?> safeNth (n -1)

data Logger a = Logger [String] a deriving (Show)

getVal :: Logger a -> a
getVal (Logger _ a) = a

getLog :: Logger a -> [String]
getLog (Logger s _) = s

nomsg :: a -> Logger a
nomsg x = Logger [] x

annotate :: String -> a -> Logger a
annotate s x = Logger [s] x

msg :: String -> Logger ()
msg s = Logger [s] ()

validateUser :: String -> Logger Bool
validateUser "1" = annotate "Valid User" True
validateUser "ninja" = nomsg True
validateUser u = annotate ("Invalid user: " ++ u) False

checkPassword :: String -> String -> Logger Bool
checkPassword "1" "pass" = annotate "Pass is ok" True
checkPassword "ninja" "pass" = annotate "Pass is ok" True
checkPassword _ pass = annotate "Pass is not ok" False

login' :: String -> String -> Logger Bool
login' user password =
  let validation = validateUser user
   in if getVal validation
        then
          let check = checkPassword user password
           in Logger (getLog validation ++ getLog check) (getVal check)
        else validation

(#>) :: Logger a -> (a -> Logger b) -> Logger b
Logger la a #> f =
  let Logger lb b = f a
   in Logger (la ++ lb) b

square :: Int -> Logger Int
square val = annotate (show val ++ "^2") (val ^ 2)

add :: Int -> Logger Int
add val = annotate (show val ++ "+1") (val + 1)

double :: Int -> Logger Int
double val = annotate (show val ++ "*1") (val * 2)

compute :: Int -> Logger Int
compute x = square x #> add #> double

(##>) :: Logger a -> Logger b -> Logger b
Logger la _ ##> Logger lb b = Logger (la ++ lb) b

filterLog :: (Eq a, Show a) => (a -> Bool) -> [a] -> Logger [a]
filterLog f [] = nomsg []
filterLog f (x : xs)
  | f x = msg ("keeping" ++ show x) ##> filterLog f xs #> (\xs' -> nomsg (x : xs'))
  | otherwise = msg ("dropping" ++ show x) ##> filterLog f xs

data Bank = Bank (Map.Map String Int) deriving (Show)

deposit :: String -> Int -> Bank -> Bank
deposit accountName amount (Bank accounts) = Bank (Map.adjust (\x -> x + amount) accountName accounts)

withdraw :: String -> Int -> Bank -> (Int, Bank)
withdraw accountName amount (Bank accounts) =
  let balance = Map.findWithDefault 0 accountName accounts
      withdrawal = min amount balance
      newAccounts = Map.adjust (\x -> x - withdrawal) accountName accounts
   in (withdrawal, Bank newAccounts)

share :: String -> String -> String -> Bank -> Bank
share from to1 to2 bank =
  let (amount, bank1) = withdraw from 100 bank
      half = div amount 2
      rest = amount - half
      bank2 = deposit to1 half bank1
      bank3 = deposit to2 rest bank2
   in bank3

data BankOp a = BankOp (Bank -> (a, Bank))

runBankOp :: BankOp a -> Bank -> (a, Bank)
runBankOp (BankOp f) bank = f bank

(+>>) :: BankOp a -> BankOp b -> BankOp b
op1 +>> op2 = BankOp combined
  where
    combined bank =
      let (_, bank1) = runBankOp op1 bank
       in runBankOp op2 bank1

(+>) :: BankOp a -> (a -> BankOp b) -> BankOp b
op +> parameterized = BankOp combined
  where
    combined bank =
      let (a, bank1) = runBankOp op bank
       in runBankOp (parameterized a) bank1

depositOp :: String -> Int -> BankOp ()
depositOp accountName amount = BankOp depositHelper
  where
    depositHelper bank = ((), deposit accountName amount bank)

withdrawOp :: String -> Int -> BankOp Int
withdrawOp accountName amount = BankOp (withdraw accountName amount)

distributeOp :: String -> String -> Int -> BankOp ()
distributeOp to1 to2 amount = depositOp to1 half +>> depositOp to2 rest
  where
    half = div amount 2
    rest = amount - half

shareOp :: String -> String -> String -> BankOp ()
shareOp from to1 to2 = withdrawOp from 100 +> distributeOp to1 to2

stealSecret''' :: String -> Maybe String
stealSecret''' password = login password >>= secret >>= decorate
  where
    decorate s = return ("Stole secret" ++ s)

increase''' :: Eq a => a -> Int -> [(a, Int)] -> Maybe [(a, Int)]
increase''' key val assocs = do
  oldVal <- lookup key assocs
  check oldVal
  return ((key, val) : delete (key, oldVal) assocs)
  where
    check x
      | val < x = Nothing
      | otherwise = return x

data State s a = State (s -> (a, s))

runState (State f) s = f s

put :: s -> State s ()
put state = State (\oldState -> ((), state))

get :: State s s
get = State (\state -> (state, state))

modify :: (s -> s) -> State s ()
modify f = State (\state -> ((), f state))

printTwoThings :: IO ()
printTwoThings = putStrLn "One" >> print 2

echo :: IO ()
echo = getLine >>= putStrLn

verboseEcho :: IO ()
verboseEcho = getLine >>= \s -> putStrLn ("You wrote :" ++ s)

query :: String -> IO String
query question = putStrLn question >> getLine

confirm :: String -> IO Bool
confirm question = putStrLn question >> fmap interpret getLine
  where
    interpret "Y" = True
    interpret _ = False
