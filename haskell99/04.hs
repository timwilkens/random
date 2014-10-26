myLength :: [a] -> Int
myLength = foldl (\a _ -> a + 1) 0

myLameLength :: [a] -> Int
myLameLength = sum . map (\_ -> 1) 
