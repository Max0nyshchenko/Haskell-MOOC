applyTo1 :: (Int -> Int) -> Int
applyTo1 f = f 1

doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

doTwice' :: (a -> a) -> a -> a
doTwice' f = f . f

c = doTwice (+ 1) 3

wrapJust xs = map Just xs

-- (f.g) x ==> f (g x)

double x = 2*x

quadruple = double . double

f = quadruple . (+1) -- computes x*(y+1)


expr1 = let notEmpty x = not (null x) in filter notEmpty [[1,2,3], [], [5]]
expr1' = filter (not . null) [[1,2,3], [], [5]]

expr2 = head (reverse "head")
expr2' = head $ reverse "head"

expr3 = reverse (map head (map reverse (["haskell ", "pro"] ++ ["dodo", "lyrict"])))
expr3' = (reverse . map head . map reverse) (["haskell ", "pro"] ++ ["dodo", "lyrict"])
expr3'' = reverse . map head . map reverse $ ["haskell ", "pro"] ++ ["dodo", "lyrict"]

findSubstring' :: String -> String -> String
findSubstring' chars = takeWhile (\x -> elem x chars) . dropWhile (\x -> not $ elem x chars)

expr4 = map (const 5) [1,2,3] -- ==> [5,5,5]
expr5 = filter id [True, False, True] -- ==> [True, True]

descend 0 = []
descend n  = n : descend (n-1)

split' :: Char -> String -> [String]
split' c [] = []
split' c xs = start : split' c (drop 1 rest)
  where start = takeWhile (/= c) xs
        rest = dropWhile (/= c) xs

myhead :: [Int] -> Int
myhead [] = -1
myhead (first:rest) = first

mytail :: [Int] -> [Int]
mytail [] = []
mytail (first:rest) = rest

sumFirstTwo :: [Integer] -> Integer
sumFirstTwo (a:b:_) = a + b
sumFirstTwo _ = 0

describeList :: [Int] -> String
describeList [] = "empty"
describeList [x] = "one elem"
describeList [x,y] = "two elem"
describeList (x:y:z:xs) = "many elems"

startsWithZero (0:xs) = True
startsWithZero _ = False

listcomper  = [first ++ " " ++ last | first <- ["jhon"], last <- ["mary"]]
listcomper'  = [reversed | word <- ["word", "amm", "askdhf"], let reversed = reverse word]
listcomper'''  = [char | (char:_) <- words "I am a Dog"]

(<+>) :: [Int] -> [Int] -> [Int]
xs <+> ys = zipWith (+) xs ys

(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b

sdf f = map (. f)

