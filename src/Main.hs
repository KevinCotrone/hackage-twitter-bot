{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Main where


import           Common
import           Control.Applicative        ((<$>), (<*>))
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception          (IOException, bracket, catch)
import           Control.Exception.Base
import           Control.Lens
import           Control.Monad              (forever, join, msum, mzero)
import           Control.Monad.IO.Class
import           Control.Monad.Reader       (ask)
import           Control.Monad.State        (get, put)
import           Data.Acid                  (AcidState, Query, Update,
                                             createCheckpoint, makeAcidic,
                                             openLocalStateFrom)
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
import           Data.Monoid
import           Data.SafeCopy              (base, deriveSafeCopy)
import qualified Data.Set                   as S
import           Data.Text
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Traversable           as T
import           Data.Word
import           Data.Yaml
import           GHC.Generics
import           Hackage.Twitter.Bot.Acid
import           Hackage.Twitter.Bot.Bitly
import           Hackage.Twitter.Bot.Types
import qualified Network.Wreq               as W
import           System.Environment
import           System.Environment
import           System.Locale
import           Text.Feed.Export
import           Text.Feed.Import
import           Text.Feed.Types
import           Text.RSS.Syntax
import           Text.XML.Light
import           Web.Twitter.Conduit


main :: IO ()
main = do
  btly <- decodeFile "config.yml" :: IO (Maybe BitLyKey)
  case btly of
    Just k -> do
      st <- openLocalStateFrom "state" (LastTimeState (betterParse (lastErrorTime k)))
      startBot k st
    Nothing -> fail "error reading bitly key"

betterParse :: String -> UTCTime
betterParse s = case (parseStupidTime s) of
                (Just t) -> t
                Nothing -> error "Unable to parse time"

startBot :: BitLyKey -> AcidState LastTimeState -> IO ()
startBot key st = forever $ do
  lastTime <- query' st PeekLastTime -- Get the time of the last post
  feedBody <- W.get "http://hackage.haskell.org/packages/recent.rss" >>= return . CBS.unpack . mconcat . toListOf W.responseBody -- Fetch the rss fedd in a bytestring and then cconcat it
  let feed = parseFeedString feedBody -- Parse the feed
  parsed <- parseFeeds $ fromJust feed -- Should be able to just
  let filteredFeeds = Prelude.filter (\post -> fullPostTime post > lastTime) $ catMaybes parsed -- filter the posts that have might have already been posted
  shortenedFeeds <- traverse (shortenURLs key) filteredFeeds                                    -- shorten all yrks
  T.traverse (\fp -> update' st (SetNewLastTime (fullPostTime fp))) (catMaybes shortenedFeeds)  -- update the acid-state when appropriate
  createCheckpoint st
  let newfeeds = S.toList . S.fromList $ Prelude.map formatPost $ catMaybes shortenedFeeds
  printWithTime $ "Current state- " ++ (show lastTime)
  if (Prelude.length newfeeds > 0) then printWithTime . show $ newfeeds else return ()
  traverse (\post -> catch (postToTwitter post) (\(e :: SomeException) -> printWithTime . show $ e)) newfeeds -- post all remaining to twitter
  threadDelay $ 60 * 1000000

printWithTime :: String -> IO ()
printWithTime t = do
  ct <- getCurrentTime
  putStrLn $ (show ct) ++ " - " ++ t

-- Use the environment variables (Look at twitter-conduit for more information)
-- in order to make a post
postToTwitter :: String -> IO ()
postToTwitter st = runTwitterFromEnv' $ do
  let status = T.pack st
  liftIO $ T.putStrLn $ "Post message: " <> status
  res <- call $ update status
  return ()
  -- liftIO $ print res

-- Attempt to read a feed into fullposts
parseFeeds :: Feed -> IO [Maybe FullPost]
parseFeeds (RSSFeed feed) = do
  return $ Prelude.map createPost $ rssItems . rssChannel $ feed

-- Take an RSS item and attempt to parse it and turn it into a post
createPost :: RSSItem -> Maybe FullPost
createPost item =
  let partialPost = join $ (\desc -> eitherToMaybe . APT.parseOnly parsePost $ pack desc) <$> (rssItemDescription item)
      title = rssItemTitle item
      link = rssItemLink item
  in (\t l part -> fromPartial t l part) <$> title <*> link <*> partialPost

-- Add a title and link to a partial post to make it a fullpost
fromPartial :: String -> String -> PartialPost -> FullPost
fromPartial title link (PartialPost auth desc time) = FullPost auth desc time title link


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (\_ -> Nothing) (Just)

-- Turn a post into the same format as the old Hackage Twitter bot
formatPost :: FullPost -> String
formatPost (FullPost author desc time title link) =
  let auth' = (Data.Char.toLower . Prelude.head $ author) : (Prelude.tail author)
      crucialLength = (Prelude.length author) + (Prelude.length title) + (Prelude.length link) + 4
      desc' = if (crucialLength + (Prelude.length desc) > 140) then trimOff crucialLength desc else desc
  in title ++ ", " ++ auth' ++ ": " ++ desc ++ " " ++ link

-- Used when a post description is too long
-- Trims off n+3 elements and adds "..." to the end
trimOff :: Int -> String -> String
trimOff n xs = Prelude.take (n + 3) xs ++ "..."

-- Used to parse the crazy time that somehow comes out of the hackage API
parseStupidTime :: String -> Maybe UTCTime
parseStupidTime = parseTime defaultTimeLocale "%a %b  %e %H:%M:%S %Z %Y"

-- Parse a partial post from the description
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
