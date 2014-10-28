dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs x
  | x <= 0 = xs
  | length xs < x = xs
  | otherwise = (take (x - 1) xs) ++ dropEvery (drop x xs) x
