myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []
