rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n
  | n == 0 = xs
  | n < 0 = rotateRight xs ((abs n) `mod` length xs)
  | otherwise = rotateLeft xs (n `mod` length xs)

rotateRight :: [a] -> Int -> [a]
rotateRight xs n =  (drop l xs) ++ (take l xs)
  where l = length xs - n

rotateLeft :: [a] -> Int -> [a]
rotateLeft xs n =  (drop n xs) ++ (take n  xs)
