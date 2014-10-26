nextToLast :: [a] -> a
nextToLast = head . tail .reverse 

safeNextToLast :: [a] -> Maybe a
safeNextToLast (x:y:[]) = Just x
safeNextToLast (x:xs) = safeNextToLast xs
safeNextToLast [] = Nothing
