import System.Directory
import System.Environment
import Data.List

directoryContents :: String -> IO [String]
directoryContents dir = do
  let dirAdjusted = if (last dir) == '/' then dir
                                         else dir ++ "/"  
  contents <- getDirectoryContents dirAdjusted
  let files = sort $ map (dirAdjusted ++) $ filter notRelative contents
              where notRelative x = (x /= "." && x /= "..")

  return files

makeDirectoryString :: [String] -> IO ()
makeDirectoryString [] = do
  return ()
makeDirectoryString (x:xs) = do
  putStrLn x
  isDirectory <- doesDirectoryExist x
  isFile <- doesFileExist x

  if isDirectory && not isFile
    then do
      showDirectory x
      makeDirectoryString xs
    else do
      makeDirectoryString xs

showDirectory :: String -> IO ()
showDirectory x = do
  contents <- directoryContents x
  makeDirectoryString contents
  
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
          showDirectory $ head args
