slice :: [a] -> Int -> Int -> [a]
slice xs start end = map fst $ filter (\(x,y) -> y >= start && y <= end) $ zip xs [1..]

slice' :: [a] -> Int -> Int -> [a]
slice' xs start end
  | start > end = []
  | otherwise = take (end - start + 1) $ (drop (start - 1) xs)
