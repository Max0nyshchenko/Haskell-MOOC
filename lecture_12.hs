map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' g (x : xs) = g x : map' g xs

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

data Tree a = Leaf | Node a (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node val left right) = Node (f val) (mapTree f left) (mapTree f right)

class Mappable m where
  mapThing :: (a -> b) -> m a -> m b

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node val l r) = Node (f val) (fmap f l) (fmap f r)

ex = reverse . tail $ "Hello"

ex1 = reverse . tail <$> Just "Hello"

ex2 = fmap (reverse . tail) (Just "Hello")

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Foldable Pair where
  foldr f initialValue (Pair x y) = f x (f y initialValue)
