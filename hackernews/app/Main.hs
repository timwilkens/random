{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Thread.Delay (delay)
import           Control.Concurrent.Async        (mapConcurrently)
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
  show EmptyItem = "Empty Response"
  show h = idString ++ " " ++ case itemType h of
    Nothing -> "Unknown item type"
    Just t -> case t of
      "story"   -> fromMaybe "" (title h) ++ " (" ++ fromMaybe "" (url h) ++ ")"
      "comment" -> renderTagsOptions renderOptions{optEscape = id} $ parseTags $ fromMaybe "" (text h)
      _ -> "Ignored"
    where idString = "[" ++ show (itemId h) ++ "]"

-- Why is this necessary?
instance Exception BL.ByteString where

data HNError = MaxItemFailure HttpException
             | MalformedMaxItem BL.ByteString
             | MalformedItemInfo
             | ItemInfoFailure HttpException
  deriving (Show)

getItemInfo :: Integer -> IO (Either HNError (String, String))
getItemInfo id =
  (process <$> (get (itemInfoUrl id) >>= asJSON)) `E.catch` handler
    where
      process item =
        let body = item ^. responseBody in
          case body of
            EmptyItem -> Left MalformedItemInfo
            _         -> Right (show body, getPrettyTimeFromItemTimeStamp $ time body)
      handler = return . Left . ItemInfoFailure

getMaxItemId :: IO (Either HNError Integer)
getMaxItemId = (process <$> get maxHackerNewsIdUrl) `E.catch` handler
  where
    process response =
      case Char8.readInteger (response ^. responseBody) of
        Nothing       -> Left (MalformedMaxItem $ response ^. responseBody)
        Just (n,rest) -> Right n
    handler = return . Left . MaxItemFailure

getPrettyTime :: UTCTime -> String
getPrettyTime = formatTime defaultTimeLocale "%c"

getPrettyTimeFromItemTimeStamp :: Maybe Integer -> String
getPrettyTimeFromItemTimeStamp t = getPrettyTime $ posixSecondsToUTCTime $ realToFrac (fromMaybe 0 t)

fetchItemChunk :: [Integer] -> IO ()
fetchItemChunk ids = fetchItemChunk' ids 0
  where
    fetchItemChunk' [] _ = return ()
    fetchItemChunk' ids failures
      | failures >= maxFailures = return ()
      | otherwise =
        mapConcurrently (\x -> do
          r <- getItemInfo x
          case r of
            Left e -> return [x]
            Right (info, prettyTime) -> do
              P.putStrLn $ prettyTime ++ " " ++ info ++ "\n"
              return []) ids >>= \ts ->
                delay microPerSecond >>
                  fetchItemChunk' (P.concat ts) (failures + 1)

fetchLoop :: Integer -> Integer -> IO ()
fetchLoop previousId failures
  | failures >= maxFailures =
      P.putStrLn "Number of consecutive failures exceeded"
  | otherwise = do
      delay (microPerSecond * 3)
      maxItemId <- getMaxItemId
      case maxItemId of
        Left e -> fetchLoop previousId (failures + 1)
        Right id
          | id == previousId -> fetchLoop previousId 0
          | otherwise -> do
              delay microPerSecond
              fetchItemChunk [(previousId+1)..id] >> fetchLoop id 0

mainLoop :: Integer -> IO ()
mainLoop failures
  | failures >= maxFailures =
      P.putStrLn "Max failures exceeded getting max item id"
  | otherwise = do
      maxItemId <- getMaxItemId
      either
        (const $ mainLoop (failures + 1))
        (\id -> fetchLoop (id-1) 0)
        maxItemId

main :: IO ()
main = do
  P.putStrLn "Starting up..."
  mainLoop 0
