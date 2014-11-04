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

sameFile :: String -> String -> IO Bool
sameFile x y = do
  file1Exists <- doesFileExist x
  file2Exists <- doesFileExist y

  if file1Exists /= file2Exists
    then do
      return False
    else if file1Exists
      then do
        file1 <- readFile x
        file2 <- readFile y
        if file1 == file2
          then do
            return True
          else do
            return False
     else do
       return False

usage :: IO ()
usage = do
  putStrLn "Usage: fast_compare dir1 dir2"
  
main = do
  args <- getArgs
  if null args
    then do
      usage
    else do
      if (length args /= 2)
        then do
          usage
        else do
          let dir1 = head args
          let dir2 = args !! 1
          dir1Exists <- doesDirectoryExist dir1
          dir2Exists <- doesDirectoryExist dir2
          if dir1Exists && dir2Exists
            then do
              showDirectory $ head args
            else do
              if not dir1Exists
                 then do
                   putStrLn $ dir1 ++ " does not exist."
                 else do
                   putStrLn $ dir2 ++ " does not exist."
