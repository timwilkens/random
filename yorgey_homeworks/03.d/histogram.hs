import Data.List

grouped :: [Integer] -> [[Integer]]
grouped [] = []
grouped xs = group $ sort $ filter (\x -> x < 10 && x >= 0) xs

addEmpty :: [[Integer]] -> [[Integer]]
addEmpty [] = []
addEmpty xs = addEmpty' xs 0

addEmpty' :: [[Integer]] -> Integer -> [[Integer]]
addEmpty' [] n
  | n <= 9 = [] : (addEmpty' [] (n + 1))
  | otherwise  = []
addEmpty' (y:ys) n
  | n > 9 = [] 
  | n == (head y) = y : (addEmpty' ys (n + 1))
  | otherwise = [] : (addEmpty' (y:ys) (n + 1))

maxLength :: [[Integer]] -> Int
maxLength [] = 0
maxLength xs = maximum $ map length xs

pad :: Int -> [[Integer]] -> [[String]]
pad 0 xs = map (map show) xs
pad _ [] = []
pad n xs = map padIt xs
  where padIt x = if length x > n  then map show x
                  else (replicate (length x) "*") ++ (replicate (n - length x) " ")

merge :: [[String]] -> [String]
merge [] = []
merge xs = map (foldl (++) []) $ transpose $ map reverse xs

squish :: [String] -> String
squish [] = []
squish xs = foldl1 (++) $ intersperse "\n" xs

base = "\n==========\n0123456789\n"

histogram :: [Integer] -> String
histogram [] = base
histogram xs = (squish $ merge $ pad maxLen $ sections) ++ base
  where sections = addEmpty $ grouped xs
        maxLen = maxLength sections
