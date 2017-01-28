{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Thread.Delay (delay)
import           Control.Exception               as E
import           Control.Lens
import           Data.Aeson
import           Data.ByteString                 as B
import           Data.ByteString.Lazy            as BL
import           Data.ByteString.Lazy.Char8      as Char8 (putStrLn,
                                                           readInteger, unpack)
import           Data.Foldable                   (forM_)
import           Data.Maybe                      (fromMaybe)
import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Clock.POSIX           (getCurrentTime,
                                                  posixSecondsToUTCTime)
import           Data.Time.Format                (defaultTimeLocale, formatTime)
import           Data.Traversable                (forM)
import           Network.HTTP.Client             (HttpException (..))
import           Network.Wreq
import           Prelude                         as P
import           Text.HTML.TagSoup               (optEscape, parseTags,
                                                  renderOptions,
                                                  renderTagsOptions)

microPerSecond = 1000000
maxFailures = 5
maxHackerNewsIdUrl = "https://hacker-news.firebaseio.com/v0/maxitem.json?print=pretty"

itemInfoUrl :: Integer -> String
itemInfoUrl id = "https://hacker-news.firebaseio.com/v0/item/" ++ show id ++ ".json"

data HNItem = HNItem  { itemId   :: Integer
                      , deleted  :: Maybe Bool
                      , itemType :: Maybe String
                      , by       :: Maybe String
                      , time     :: Maybe Integer
                      , text     :: Maybe String
                      , dead     :: Maybe Bool
                      , parent   :: Maybe Integer
                      , kids     :: Maybe [Integer]
                      , url      :: Maybe String
                      , score    :: Maybe Integer
                      , title    :: Maybe String }
            | EmptyItem

instance FromJSON HNItem where
  parseJSON (Object o) =
    HNItem <$> o .: "id"
           <*> o .:? "deleted"
           <*> o .:? "type"
           <*> o .:? "by"
           <*> o .:? "time"
           <*> o .:? "text"
           <*> o .:? "dead"
           <*> o .:? "parent"
           <*> o .:? "kids"
           <*> o .:? "url"
           <*> o .:? "score"
           <*> o .:? "title"
  parseJSON Null = pure EmptyItem

instance Show HNItem where
  show EmptyItem = "No response"
  show h = idString ++ " " ++ case itemType h of
    Nothing -> "Unknown item type"
    Just t -> case t of
      "story"   -> fromMaybe "" (title h) ++ " (" ++ fromMaybe "" (url h) ++ ")"
      "comment" -> renderTagsOptions renderOptions{optEscape = id} $ parseTags $ fromMaybe "" (text h)
      _ -> "Ignored"
    where idString = "[" ++ show (itemId h) ++ "]"

-- Why is this necessary?
instance Exception BL.ByteString where

data HNError = FailedToGetMaxItem HttpException
             | InvalidMaxItemResponse BL.ByteString
             | FailedToGetItemInfo HttpException
  deriving (Show)

getItemInfo :: Integer -> IO (Either HNError (String, String))
getItemInfo id = do
  now <- getCurrentTime
  (process now <$> (get (itemInfoUrl id) >>= asJSON)) `E.catch` handler
    where
      process now item =
        let body = item ^. responseBody in
          case body of
            EmptyItem -> Right (show body, getPrettyTime now)
            _         -> Right (show body, getPrettyTimeFromItemTimeStamp $ time body)
      handler = return . Left . FailedToGetItemInfo

getMaxItemId :: IO (Either HNError Integer)
getMaxItemId = (process <$> get maxHackerNewsIdUrl) `E.catch` handler
  where
    process response =
      case Char8.readInteger (response ^. responseBody) of
          Nothing       -> Left (InvalidMaxItemResponse $ response ^. responseBody)
          Just (n,rest) -> Right n
    handler = return . Left . InvalidMaxItemResponse

getPrettyTime :: UTCTime -> String
getPrettyTime = formatTime defaultTimeLocale "%c"

getPrettyTimeFromItemTimeStamp :: Maybe Integer -> String
getPrettyTimeFromItemTimeStamp t = getPrettyTime $ posixSecondsToUTCTime $ realToFrac (fromMaybe 0 t)

fetchItemChunk :: Integer -> Integer -> IO Integer
fetchItemChunk low high =
  sum <$> forM [low..high] (\x -> do
    r <- getItemInfo x
    case r of
      Left e -> do
        print e
        return 1 :: IO Integer
      Right (info, prettyTime) -> do
        P.putStrLn $ prettyTime ++ " " ++ info
        P.putStrLn ""
        return 0)

fetchLoop :: Integer -> Integer -> IO ()
fetchLoop previousId numFailures =
  if numFailures >= maxFailures
    then P.putStrLn "Number of consecutive failures exceeded"
    else do
      delay (microPerSecond * 3)
      maxItemId <- getMaxItemId
      case maxItemId of
        Left e -> do
          print e
          fetchLoop previousId (numFailures + 1)
        Right id ->
          if id == previousId
            then fetchLoop previousId 0
            else do
              delay microPerSecond
              failures <- fetchItemChunk (previousId+1) id
              fetchLoop id failures

main' :: Integer -> IO ()
main' failures =
  if failures >= maxFailures
    then P.putStrLn "Max failures exceeded getting max item id"
    else do
      maxItemId <- getMaxItemId
      case maxItemId of
        Left e   -> do
          print e
          main' (failures + 1)
        Right id -> fetchLoop (id-1) 0

main :: IO ()
main = do
  P.putStrLn "Starting up..."
  main' 0
