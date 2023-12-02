import Control.Monad
import Data.IORef

--import Network.HTTP

questionnaire = do
  putStrLn "Write Something"
  s <- getLine
  putStrLn ("You wrote: " ++ s)

--httpMain = do
--  rsp <- simpleHTTP (getRequest "http://httpbin.org/base64/aGFza2VsbCBmb3IgZXZlcgo=")
--  body <- getResponseBody rsp
--  forM_ (words body) $ \w -> do
--    putStr "word: "
--    putStrLn w

query :: IO ()
query = do
  putStrLn "Write something!"
  s <- getLine
  let n = length s
  putStrLn ("You wrote " ++ show n ++ " characters")

askForALine :: IO String
askForALine = do
  putStrLn "Please give me a line"
  getLine

ask :: String -> IO String
ask question = do
  putStrLn question
  getLine

produceThree :: IO Int
produceThree = return 3

printThree :: IO ()
printThree = do
  three <- produceThree
  putStrLn (show three)

yesNoQuestion :: String -> IO Bool
yesNoQuestion question = do
  putStrLn question
  s <- getLine
  return (s == "Y")

produceTwo :: IO Int
produceTwo = do
  return 1
  return 2

printDescription :: Int -> IO ()
printDescription n
  | even n = putStrLn "even"
  | n == 3 = putStrLn "three"
  | otherwise = print n

printList :: [Int] -> IO ()
printList [] = return ()
printList (x : xs) = do
  print x
  printList xs

readAndSum :: Int -> IO Int
readAndSum 0 = return 0
readAndSum n = do
  i <- readLn
  s <- readAndSum (n -1)
  return (i + s)

printList' :: [Int] -> IO ()
printList' xs = mapM_ print xs

readAndSum' :: Int -> IO Int
readAndSum' n = do
  numbers <- replicateM n readLn
  return (sum numbers)

ask' :: [String] -> IO [String]
ask' questions = do
  forM questions askOne

askOne :: String -> IO String
askOne question = do
  putStr question
  putStrLn "?"
  getLine

sumList :: [Int] -> IO Int
sumList xs = do
  r <- newIORef 0
  forM_ xs (\x -> modifyIORef r (x +))
  readIORef r
