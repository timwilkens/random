range :: (Ord a, Num a) => a -> a -> [a]
range x y
  | x > y = x:(range (x - 1) y)
  | x == y = [x]
  | otherwise = x:(range (x + 1) y)
