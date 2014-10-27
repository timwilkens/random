makeTriples :: (Ord a) => [a] -> [(a,a,a)]
makeTriples (x:y:z:xs) = (x,y,z) : makeTriples (y:(z:xs))
makeTriples _ = []

isLocal :: (Ord a) => (a,a,a) -> Bool
isLocal (x,y,z) = if y > x && y > z then True
                  else False

localMaxima :: (Ord a) => [a] -> [a]
localMaxima xs = map (\(a,b,c) -> b) $ filter isLocal $ makeTriples xs
