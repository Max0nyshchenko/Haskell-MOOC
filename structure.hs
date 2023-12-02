module Gold where

phi :: Double
phi = (sqrt 5 + 1) / 2

polynomial :: Double -> Double
polynomial x = x^2 - x - 1

f x = polynomial (polynomial x)

main = do
  print (polynomial phi)
  print (f phi)

checkPassword password = if password == "swordship"
                         then "You're in."
                         else "ACCESS DENIED!"

absoluteValue n = if n < 0 then -n else n

login user pass = if user == "unicorn"
                  then if pass == "f3bulos"
                       then "Unicord logged in"
                       else "wrong password"
                  else "unknown user"

circleArea :: Double -> Double
circleArea r = pi * rsquare
  where pi = 3.1415926
        rsquare = r * r

circleArea' :: Double -> Double 
circleArea' r = let pi = 3.1415926
                    rsquare = r * r
                in pi * rsquare

x :: Int
x = 4

fn :: Int -> Int
fn x = 2 * x

g :: Int -> Int
g y = x where x = 6

h :: Int -> Int
h x = x where x = 3

greet :: String -> String -> String
greet "Finland" name = "Hei, " ++ name
greet "Italy" name = "ciao, " ++ name
greet "England" name = "How do you do, " ++ name
greet _ name = "Hello, " ++ name

login' :: String -> String -> String
login' "unicorn89" "fagdgdf8" = "unicorn89 logged in"
login' "unicorn89" _ = "wrong password"
login' _ _ = "unknown user"


factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)
