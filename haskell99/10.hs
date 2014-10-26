import Data.List

runLength :: (Eq a) => [a] -> [(Int, a)]
runLength = map (\x -> (length x, head x)) . group
