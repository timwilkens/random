myLast :: [a] -> a
myLast = foldr1 (\_ x -> x)

myBuiltIn :: [a] -> a
myBuiltIn = last

mySafeLast :: [a] -> Maybe a
mySafeLast [] = Nothing
mySafeLast xs = Just $ last xs
