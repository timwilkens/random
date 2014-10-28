removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt _ [] = Nothing
removeAt n xs
  | n <= 0 = Nothing
  | length xs < n = Nothing
  | otherwise = Just (last front, init front ++ back)
    where (front, back) = splitAt n xs
