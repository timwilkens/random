module HW02 where

import Words
import Data.List
import Data.Char

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy _ [] = False
formableBy "" _ = True
formableBy (x:end) xs
  | x `elem` xs = formableBy end (delete x xs)
  | otherwise = False

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate x hand y
  | length x /= length y = False
  | x == y = True
  | otherwise = checkPairs (zip x y) hand

checkPairs :: [(Char,Char)] -> Hand -> Bool
checkPairs [] _ = True
checkPairs ((a,b):xs) hand
  | a == b = checkPairs xs hand
  | a == '?' && b `elem` hand = checkPairs xs (delete b hand)
  | otherwise = False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate t hand = filter (\x -> wordFitsTemplate t hand x ) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . map scrabbleValue

bestWords :: [String] -> [String]
bestWords [] = []
bestWords xs = bestWords' xs [] 0

bestWords' :: [String] -> [String] -> Int -> [String]
bestWords' [] acc _ = acc
bestWords' (x:xs) acc n
  | value > n = bestWords' xs [x] value
  | value == n = bestWords' xs (x:acc) value
  | otherwise = bestWords' xs acc n
    where value = scrabbleValueWord x

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate t x = compute (zip t x) 0 1

compute :: [(Char, Char)] -> Int -> Int -> Int
compute [] n m = n * m
compute ((a,b):xs) n m
  | a == '?' || a == b = compute xs (n + value) m
  | a == 'T' = compute xs (n + (value * 3)) m
  | a == 'D' = compute xs (n + (value * 2)) m
  | a == '2' = compute xs (n + value) 2
  | a == '3' = compute xs (n + value) 3
    where value = scrabbleValue b
