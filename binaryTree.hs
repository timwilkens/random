data Tree a = Node (Tree a) a (Tree a)
            | Empty
  deriving (Show, Eq)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

treeFromList :: Ord a => [a] -> Tree a
treeFromList [] = Empty
treeFromList xs = foldr addItem Empty xs

addItem :: Ord a => a -> Tree a -> Tree a
addItem x Empty = Node Empty x Empty
addItem x t@(Node left y right)
  |y < x = Node left y (addItem x right)
  |y > x = Node (addItem x left) y right
  |otherwise = t

treeElem :: Ord a => a -> Tree a -> Bool
treeElem x Empty = False
treeElem x (Node left y right)
  | x == y = True
  | x < y = treeElem x left
  | x > y = treeElem x right

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeToList' :: Tree a -> [a]
treeToList' = treeFold [] (\l x r -> l ++ [x] ++ r)

numberOfNodes' :: Tree a -> Integer
numberOfNodes' = treeFold 0 (\l _ r -> l + 1 + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> l + x + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

treeMin :: (Ord a, Bounded a) => Tree a -> a
treeMin = treeFold maxBound (\l x r -> l `min` x `min` r)

treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l x r -> l `max` x `max` r)

sameTree :: Eq a => Tree a -> Tree a -> Bool
sameTree Empty Empty = True
sameTree (Node l1 x1 r1) (Node l2 x2 r2)
  | x1 == x2 = (sameTree l1 l2) && (sameTree r1 r2)
  | otherwise = False
sameTree _ _ = False

