printTwoThings :: IO ()
printTwoThings = do
  putStrLn "Hello!"
  putStrLn "How are you!"

greet :: IO ()
greet = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("hello, " ++ name)
