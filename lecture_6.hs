data Color = Black | White

instance Eq Color where
  Black == Black = True
  White == White = True
  _ == _  = False

class Size a where
  size :: a -> Int

instance Size Int where
  size x = abs x

instance Size [a] where
  size x = length x

class Example a where
  example :: a
  examples :: [a]
  examples = [example]

instance Example Int where
  example = 1
  examples = [0,1,2]

instance Example Bool where
  example = True

class Combine a where
  combine :: a -> a -> a
  combine3 :: a -> a -> a -> a
  combine3 x y z = combine x (combine y z)

class CombineSecond a where
  combineSecond :: a -> a -> a

combine3Second :: CombineSecond a => a -> a -> a -> a
combine3Second x y z = combineSecond x (combineSecond y z)

data IntPair = IntPair Int Int
  deriving Show

instance Eq IntPair where
  IntPair a1 a2 == IntPair b1 b2 = a1==b1 && b1==b2

instance Ord IntPair where
  IntPair a1 a2 <= IntPair b1 b2
    | a1 < b1 = True
    | a1 > b1 = False
    | otherwise = a2 <= b2

data Pair a = MakePair a a deriving Show

instance Eq a => Eq (Pair a) where
  (MakePair x y) == (MakePair a b) = x==a && y==b

class Check a where
  check :: a -> Bool

instance Check Int where
  check x = x > 0

instance Check a => Check [a] where
  check xs = and (map check xs)

checkAll :: Check a => [a] -> Bool
checkAll xs = and (map check xs)

class Size a => SizeBoth a where
  sizeBoth :: a -> a -> Int
  sizeBoth x y = size x + size y
