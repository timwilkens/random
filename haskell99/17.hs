-- replacing the built-ins
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs x
  | x > length xs = (xs, [])
  | otherwise = ((myTake x xs), (myDrop x xs))

myTake :: Int -> [a] -> [a]
myTake x xs = map fst $ filter (\(a, b) -> b <= x) $ zip xs [1..]

myDrop :: Int -> [a] -> [a]
myDrop x xs = map fst $ filter (\(a,b) -> b > x) $ zip xs [1..]

split' :: [a] -> Int -> ([a], [a])
split' [] _ = ([], [])
split' el@(x:xs) n
  | n > 0 = ((x:ys), zs)
  | otherwise = ([], el)
    where (ys, zs) = split' xs (n - 1)
