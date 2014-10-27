import Data.List

data Encoded a = Multiple Int a
               | Single a
  deriving Show

runLength :: (Eq a) => [a] -> [Encoded a]
runLength = map makeEncoded . group

makeEncoded :: [a] -> Encoded a
makeEncoded [] = error "Called on empty listw"
makeEncoded xs
  | length xs == 1 = Single (head xs)
  | otherwise = Multiple (length xs) (head xs)
