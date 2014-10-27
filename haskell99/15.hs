dupli :: [a] -> Int -> [a]
dupli xs n = concatMap (replicate n) xs
