{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where


import           Control.Applicative        ((<$>), (<*>))
import           Control.Exception          (bracket)
import           Control.Lens
import           Control.Monad              (join, msum)
import           Control.Monad.Reader       (ask)
import           Control.Monad.State        (get, put)
import           Data.Acid                  (AcidState, Query, Update,
                                             makeAcidic, openLocalStateFrom)
import           Data.Acid.Advanced         (query', update')
import           Data.Acid.Local            (createCheckpointAndClose)
import qualified Data.Attoparsec.Text       as APT
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as CBS
import           Data.Data                  (Data, Typeable)
import           Data.Maybe
import           Data.Monoid
import           Data.SafeCopy              (base, deriveSafeCopy)
import           Data.Text
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Traversable           as T
import           Data.Word
import qualified Network.Wreq               as W
import           System.Environment
import           System.Locale
import           Text.Feed.Export
import           Text.Feed.Import
import           Text.Feed.Types
import           Text.RSS.Syntax
import           Text.XML.Light
import Data.Time.Calendar

data LastTimeState = LastTimeState { lastTime :: UTCTime }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- $(deriveSafeCopy 0 ' base ''UTCTime)

$(deriveSafeCopy 0 'base ''LastTimeState)

initialLastTimeState :: IO LastTimeState
initialLastTimeState = do
  ct <- getCurrentTime
  return $ LastTimeState ct

setNewLastTime :: UTCTime -> Update LastTimeState UTCTime
setNewLastTime time = do
     put $ LastTimeState time
     return time

peekLastTime :: Query LastTimeState UTCTime
peekLastTime = lastTime <$> ask

$(makeAcidic ''LastTimeState ['setNewLastTime, 'peekLastTime])

main :: IO ()
main = do
  iState <- initialLastTimeState
  st <- openLocalStateFrom "state" iState
  lastTime <- query' st PeekLastTime
  feedBody <- W.get "http://hackage.haskell.org/packages/recent.rss" >>= return . CBS.unpack . mconcat . toListOf W.responseBody
  let feed = parseFeedString feedBody
  parsed <- parseFeeds $ fromJust feed
  putStrLn . show $ Prelude.filter (\post -> postTime post < lastTime) $ catMaybes parsed
  return ()

parseFeeds :: Feed -> IO [Maybe Post]
parseFeeds (RSSFeed feed) = do
  return $ Prelude.map createPost $ rssItems . rssChannel $ feed


createPost :: RSSItem -> Maybe Post
createPost item =join $ (\desc -> eitherToMaybe . APT.parseOnly parsePost $ pack desc) <$> (rssItemDescription item)


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (\_ -> Nothing) (Just)

data Post = Post {author :: String , description :: String, postTime :: UTCTime} deriving (Show)


parseStupidTime :: String -> Maybe UTCTime
parseStupidTime = parseTime defaultTimeLocale "%a %b  %e %H:%M:%S %Z %Y"

parsePost :: APT.Parser Post
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
      (Just time) ->  return $ Post (unpack author) (unpack description) time
      _ -> fail "Error reading time"

-- "<i>Added by BryanOSullivan, Sat Jul  5 05:57:18 UTC 2014.</i><p>Pure and impure Bloom Filter implementations."

