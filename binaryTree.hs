data Tree a = Node (Tree a) a (Tree a)
            | Empty
  deriving (Show, Read, Eq)

makeTree :: a -> Tree a
makeTree x = Node Empty x Empty

treeFromList :: Ord a => [a] -> Tree a
treeFromList [] = Empty
treeFromList (x:xs) = treeFromList' (makeTree x) xs

treeFromList' :: Ord a => Tree a -> [a] -> Tree a
treeFromList' t [] = t
treeFromList' t (x:xs) = treeFromList' (addItem x t) xs

addItem :: Ord a => a -> Tree a -> Tree a
addItem x Empty = Node Empty x Empty
addItem y t@(Node left x right)
  |x < y = Node left x (addItem y right)
  |x > y = Node (addItem y left) x right
  |otherwise = t

treeElem :: Ord a => a -> Tree a -> Bool
treeElem x Empty = False
treeElem x (Node left y right)
  | x == y = True
  | x < y = treeElem x left
  | x > y = treeElem y right

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
  | x1 == x2 = (sameTree l1 l2) && (sameTree l2 r2)
  | otherwise = False
sameTree _ _ = False

height :: Tree a -> Int
height t = height' 0 t

height' :: Int -> Tree a -> Int
height' x Empty = x
height' x (Node left _ right) =
  max (height' (x + 1) left) (height' (x + 1) right)
