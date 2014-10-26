import Data.List

-- Cheater way
myPack :: (Eq a) => [a] -> [[a]]
myPack = group

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:matches) : (pack ys)
  where matches = takeWhile (== x) xs
        ys = dropWhile (== x) xs
