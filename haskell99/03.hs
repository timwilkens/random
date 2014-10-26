elementAt :: [a] -> Int -> a
elementAt [] _ = error "exceeded bounds of list"
elementAt (x:xs) n
  | n < 1 = error "index out of bounds"
  | n == 1 = x
  | otherwise = elementAt xs $ n - 1
