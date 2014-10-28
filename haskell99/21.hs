insertAt :: a -> [a] -> Int -> Maybe [a]
insertAt x [] _ = Just [x]
insertAt x xs n
  | n <= 0 = Nothing
  | n == 1 = Just (x:xs)
  | n > length xs = Just (xs ++ [x])
  | otherwise = Just ((take (n - 1) xs) ++ (x:(drop (n - 1) xs)))

insertAt' :: a -> [a] -> Int -> Maybe [a]
insertAt' x [] _ = Just [x]
insertAt' _ _ n | n <= 0 = Nothing
insertAt' x (y:ys) n = Just (addIt x (y:ys) n)
  where
    addIt x (y:ys) n
      | n == 1 = x:(y:ys)
      | otherwise = y:(addIt x ys (n - 1))
