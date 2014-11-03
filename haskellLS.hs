import System.Directory
import System.Environment
import Data.List

data Item = Directory String
          | File String

directoryContents :: String -> IO [String]
directoryContents dir = do
  let dirAdjusted = if (last dir) == '/' then dir
                                         else dir ++ "/"  
  contents <- getDirectoryContents dirAdjusted
  let files = map (dirAdjusted ++) $ filter notRelative contents
              where notRelative x = (x /= "." && x /= "..")

  return files

makeItemList :: [String] -> IO [Item]
makeItemList [] = do
  return []
makeItemList (x:xs) = do
  i <- makeItem x
  end <- makeItemList xs
  return (i:end)

makeItem :: String -> IO Item
makeItem x = do
  fileExists <- doesFileExist x
  dirExists <- doesDirectoryExist x
  if fileExists && not dirExists
    then do
      return $ File x
    else if dirExists && not fileExists
      then do
        return $ Directory x
	else do
      error $ "Bad item " ++ x

makeDirectoryString :: [Item] -> IO String
makeDirectoryString [] = do
  return []
makeDirectoryString (File x:xs) = do
  rest <- makeDirectoryString xs
  return (x ++ "\n" ++ rest)
makeDirectoryString (Directory x:xs) = do
  current <- showDirectory x
  rest <- makeDirectoryString xs
  return (x ++ "\n" ++ current ++ rest)

showDirectory :: String -> IO String
showDirectory x = do
  contents <- directoryContents x
  items <- makeItemList contents
  y <- makeDirectoryString items
  return y
  
main = do
  args <- getArgs
  if null args
    then do
      putStrLn "Must provide directory."
    else do
      if (length args /= 1)
        then do
          putStrLn "Only one argument allowed."
        else do
          dirString <- showDirectory $ head args
          putStr dirString
