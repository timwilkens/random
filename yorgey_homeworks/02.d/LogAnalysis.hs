{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage m = toMTuple $ words m

toMTuple :: [String] -> LogMessage
toMTuple (x:xs) = toLogMessage (x, xs)
toMTuple _ = toLogMessage ("Error", ["Error"])

toLogMessage :: (String, [String]) -> LogMessage
toLogMessage ("I",n:rest) = LogMessage Info (read n) (unwords rest)
toLogMessage ("W",n:rest) = LogMessage Warning (read n) (unwords rest)
toLogMessage ("E",n:m:rest) = LogMessage (Error $ read n) (read m) (unwords rest)
toLogMessage (x,xs) = Unknown (unwords $ x:xs)

parse :: String -> [LogMessage]
parse xs = map(parseMessage) $ lines xs

insert :: LogMessage -> MessageTree -> MessageTree
insert m tree = case m of
                    Unknown _ -> tree
                    _ -> insert' m tree

insert' :: LogMessage -> MessageTree -> MessageTree
insert' m@(LogMessage _ time _) mTree@(Node lTree base@(LogMessage _ time' _) rTree)
  | time > time' = Node lTree base (insert' m rTree)
  | time < time' = Node (insert' m lTree) base rTree
  | otherwise = mTree
insert' m Leaf = Node Leaf m Leaf
insert' _ tree = tree

build :: [LogMessage] -> MessageTree
build xs = build' xs Leaf

build' :: [LogMessage] -> MessageTree -> MessageTree
build' (x:xs) tree = build' xs $ insert x tree
build' [x] tree = insert x tree
build' [] tree = tree

inOrder :: MessageTree -> [LogMessage]
inOrder (Node lTree message rTree) = inOrder(lTree) ++ [message] ++ inOrder(rTree) 
inOrder Leaf = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = justMessages $ inOrder $ build $ over50 $ justErrors xs
whatWentWrong [] = []

justMessages :: [LogMessage] -> [String]
justMessages ((LogMessage (Error _) _ m):xs) = m:(justMessages xs)
justMessages [(LogMessage (Error _) _ m)] = [m]
justMessages _ = []

over50 :: [LogMessage] -> [LogMessage]
over50 (m@(LogMessage (Error a) _ _):xs)
  | a >= 50 = m : (over50 xs)
  | otherwise = over50 xs
over50 [m@(LogMessage (Error a) _ _)]
  | a >= 50 = [m]
  | otherwise = []
over50 _ = []

justErrors :: [LogMessage] -> [LogMessage]
justErrors (x:xs) = case x of
                     LogMessage (Error _) _ _ -> x:(justErrors xs)
                     _ -> justErrors xs
justErrors [x] = case x of
                  LogMessage (Error _) _ _ -> [x]
                  _ -> []
justErrors _ = []
