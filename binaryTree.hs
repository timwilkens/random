data Tree a = Node (Tree a) a (Tree a)
            | Empty
  deriving (Show, Eq)

leaf :: Ord a => a -> Tree a
leaf x = Node Empty x Empty

treeFromList :: Ord a => [a] -> Tree a
treeFromList [] = Empty
treeFromList (x:xs) = treeFromList' (leaf x) xs

treeFromList' :: Ord a => Tree a -> [a] -> Tree a
treeFromList' t [] = t
treeFromList' t (x:xs) = treeFromList' (addItem x t) xs

addItem :: Ord a => a -> Tree a -> Tree a
addItem x Empty = leaf x
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

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Node left x right) = (treeToList left) ++ [x] ++ (treeToList right)

numberOfNodes :: Tree a -> Integer
numberOfNodes Empty = 0
numberOfNodes (Node l _ r) = 1 + (numberOfNodes l) + (numberOfNodes r)

treeSum :: Tree Integer -> Integer
treeSum Empty = 0
treeSum (Node l x r) = x + (treeSum l) + (treeSum r)

treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + (treeDepth l) + (treeDepth r)

minItem :: Tree a -> Maybe a
minItem Empty = Nothing
minItem (Node Empty x _) = Just x
minItem (Node left x _) = minItem left

maxItem :: Tree a -> Maybe a
maxItem Empty = Nothing
maxItem (Node _ x Empty) = Just x
maxItem (Node _ x right) = maxItem right

sameTree :: Eq a => Tree a -> Tree a -> Bool
sameTree Empty Empty = True
sameTree (Node l1 x1 r1) (Node l2 x2 r2)
  | x1 == x2 = (sameTree l1 l2) && (sameTree r1 r2)
  | otherwise = False
sameTree _ _ = False

