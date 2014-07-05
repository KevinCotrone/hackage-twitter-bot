{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables               #-}


module Main where


import           Control.Applicative        ((<$>), (<*>))
import           Control.Exception          (bracket, catch, IOException)
import           Control.Lens
import           Control.Monad              (join, msum, mzero, forever)
import           Control.Monad.Reader       (ask)
import           Control.Monad.State        (get, put)
import           Data.Acid                  (AcidState, Query, Update, createCheckpoint,
                                             makeAcidic, openLocalStateFrom)
import           Data.Acid.Advanced         (query', update')
import           Data.Acid.Local            (createCheckpointAndClose)
import           Data.Aeson
import qualified Data.Attoparsec.Text       as APT
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as CBS
import           Data.Char
import           Data.Data                  (Data, Typeable)
import           Data.Maybe
import           Data.Monoid
import           Data.SafeCopy              (base, deriveSafeCopy)
import           Data.Text
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Traversable           as T
import           Data.Word
import           GHC.Generics
import qualified Network.Wreq               as W
import           System.Environment
import           System.Locale
import           Text.Feed.Export
import           Text.Feed.Import
import           Text.Feed.Types
import           Text.RSS.Syntax
import           Text.XML.Light
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class
import Web.Twitter.Conduit
import System.Environment
import Common
import Control.Exception.Base
import Control.Concurrent
import Control.Concurrent.Async
import  Data.Yaml

data LastTimeState = LastTimeState { lastTime :: UTCTime }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- $(deriveSafeCopy 0 ' base ''UTCTime)

$(deriveSafeCopy 0 'base ''LastTimeState)


data BitLyData a = BitLyData {bitlyData :: a} deriving (Show)
data BitLyURL = BitLyURL {bitLyUrl :: String} deriving (Show)
data BitLyKey = BitLyKey {bitLyKey :: String} deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (BitLyData a) where
  parseJSON (Object o) = BitLyData <$> o .: "data"
  parseJSON _ = mzero

instance FromJSON BitLyKey where

instance FromJSON BitLyURL where
  parseJSON (Object o) = BitLyURL <$> o .: "url"
  parseJSON _ = mzero



initialLastTimeState :: LastTimeState
initialLastTimeState =  LastTimeState $ UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

setNewLastTime :: UTCTime -> Update LastTimeState UTCTime
setNewLastTime time = do
  c@LastTimeState{..} <- get
  let time' = max time lastTime
  put $ LastTimeState time'
  return time'

peekLastTime :: Query LastTimeState UTCTime
peekLastTime = lastTime <$> ask

$(makeAcidic ''LastTimeState ['setNewLastTime, 'peekLastTime])

main :: IO ()
main = do
  st <- openLocalStateFrom "state" initialLastTimeState
  btly <- decodeFile "config.yml" :: IO (Maybe BitLyKey)
  case btly of
    Just k -> startBot k st
    Nothing -> fail "error reading bitly key"


startBot :: BitLyKey -> AcidState LastTimeState -> IO ()
startBot key st = forever $ do
  lastTime <- query' st PeekLastTime
  putStrLn "Getting body"
  feedBody <- W.get "http://hackage.haskell.org/packages/recent.rss" >>= return . CBS.unpack . mconcat . toListOf W.responseBody
  let feed = parseFeedString feedBody
  putStrLn "Parsing feeds"
  parsed <- parseFeeds $ fromJust feed
  let filteredFeeds = Prelude.filter (\post -> fullPostTime post > lastTime) $ catMaybes parsed
  putStrLn "Getting body"
  shortenedFeeds <- traverse (shortenURLs key) filteredFeeds
  T.traverse (\fp -> update' st (SetNewLastTime (fullPostTime fp))) (catMaybes shortenedFeeds)
  createCheckpoint st
  putStrLn . show . Prelude.map formatPost $ catMaybes shortenedFeeds
  traverse (\post -> catch (postToTwitter . formatPost $ post) (\(e :: SomeException) -> putStrLn . show $ e)) $ catMaybes shortenedFeeds
  threadDelay $ 15 * 1000000

postToTwitter :: String -> IO ()
postToTwitter st = runTwitterFromEnv' $ do
  let status = T.pack st
  liftIO $ T.putStrLn $ "Post message: " <> status
  res <- call $ update status
  liftIO $ print res

parseFeeds :: Feed -> IO [Maybe FullPost]
parseFeeds (RSSFeed feed) = do
  return $ Prelude.map createPost $ rssItems . rssChannel $ feed


createPost :: RSSItem -> Maybe FullPost
createPost item =
  let partialPost = join $ (\desc -> eitherToMaybe . APT.parseOnly parsePost $ pack desc) <$> (rssItemDescription item)
      title = rssItemTitle item
      link = rssItemLink item
  in (\t l part -> fromPartial t l part) <$> title <*> link <*> partialPost


fromPartial :: String -> String -> PartialPost -> FullPost
fromPartial title link (PartialPost auth desc time) = FullPost auth desc time title link


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (\_ -> Nothing) (Just)

data FullPost = FullPost { fullPostAuthor :: String, fullPostDescription :: String, fullPostTime :: UTCTime, fullPostTitle :: String, fullPostLink :: String} deriving (Show)

data PartialPost = PartialPost {author :: String , description :: String, postTime :: UTCTime} deriving (Show)

formatPost :: FullPost -> String
formatPost (FullPost author desc time title link) =
  let auth' = (Data.Char.toLower . Prelude.head $ author) : (Prelude.tail author)
      crucialLength = (Prelude.length author) + (Prelude.length title) + (Prelude.length link) + 4
      desc' = if (crucialLength + (Prelude.length desc) > 140) then trimOff crucialLength desc else desc
  in title ++ ", " ++ auth' ++ ": " ++ desc ++ " " ++ link

trimOff :: Int -> String -> String
trimOff n xs = Prelude.take (n + 3) xs ++ "..."



parseStupidTime :: String -> Maybe UTCTime
parseStupidTime = parseTime defaultTimeLocale "%a %b  %e %H:%M:%S %Z %Y"

parsePost :: APT.Parser PartialPost
parsePost = do
    APT.string "<i>"
    author <- APT.takeWhile (\c -> c /= ',')
    APT.take 2
    sDate <- APT.takeWhile (\c -> c /= '.')
    APT.skipWhile (\c -> c /= '>')
    APT.take 1
    APT.skipWhile (\c -> c /= '>')
    APT.take 1
    description <- APT.takeText
    case (parseStupidTime . unpack $ sDate) of
      (Just time) ->  return $ PartialPost (unpack author) (unpack description) time
      _ -> fail "Error reading time"

shortenURLs :: BitLyKey -> FullPost -> IO (Maybe FullPost)
shortenURLs k p = do
  link' <- shortenURL k (fullPostLink p)
  case link' of
    (Just l) -> return . Just $ (p {fullPostLink = l})
    Nothing -> return Nothing

shortenURL :: BitLyKey -> String -> IO (Maybe String)
shortenURL key longURL = do
  resp <- W.asJSON =<< W.get ("https://api-ssl.bitly.com" ++ "/v3/shorten?access_token=" ++ (bitLyKey key)  ++ "&longUrl=" ++ longURL )
  return $ bitLyUrl . bitlyData <$> resp ^? W.responseBody