import Data.List

data Encoded a = Multiple Int a
               | Single a
  deriving Show

decodeRunLength :: (Eq a) => [Encoded a] -> [a]
decodeRunLength = concatMap decode

decode :: Encoded a -> [a]
decode xs = case xs of
  Single x -> [x]
  Multiple n x -> replicate n x

runLength :: (Eq a) => [a] -> [Encoded a]
runLength = map makeEncoded . group

makeEncoded :: [a] -> Encoded a
makeEncoded [] = error "Called on empty listw"
makeEncoded xs
  | length xs == 1 = Single (head xs)
  | otherwise = Multiple (length xs) (head xs)
