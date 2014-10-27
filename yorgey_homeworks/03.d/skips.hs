eachN :: Int -> [a] -> [a]
eachN 0 _ = []
eachN n xs = if n > length xs then []
             else (xs !! (n - 1)) : eachN n (drop n xs)

skips :: [a] -> [[a]]
skips xs = map (\x -> eachN x xs) [1 .. length xs]
