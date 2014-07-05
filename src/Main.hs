{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Applicative
import           Control.Lens
import           Control.Monad
import qualified Data.Attoparsec.Text       as APT
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as CBS
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Traversable           as T
import           Data.Word
import           Network.Wreq
import           System.Environment
import           Text.Feed.Export
import           Text.Feed.Import
import           Text.Feed.Types
import           Text.RSS.Syntax
import           Text.XML.Light
import System.Locale

main :: IO ()
main = do
  feedBody <- get "http://hackage.haskell.org/packages/recent.rss" >>= return . CBS.unpack . mconcat . toListOf responseBody
  let feed = parseFeedString feedBody
  T.sequence $ postFeed <$> feed
  return ()

postFeed :: Feed -> IO ()
postFeed (RSSFeed feed) = do
  putStrLn . show . Prelude.map createPost $ rssItems . rssChannel $ feed


createPost :: RSSItem -> Maybe String
createPost item =
    let title = rssItemTitle item
        mAuthorDesc = join $ (\desc -> eitherToMaybe . APT.parseOnly parseAuthorAndDescription $ pack desc) <$> (rssItemDescription item)
    in (\title (author,_,desc) -> title ++ ", " ++ author ++ ": " ++ desc) <$> title <*> mAuthorDesc


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (\_ -> Nothing) (Just)

data AuthorDescription = AuthorDescription {author :: Text , description :: Text} deriving (Show)


parseStupidTime :: String -> Maybe UTCTime
parseStupidTime = parseTime defaultTimeLocale "%a %b  %e %H:%M:%S %Z %Y"

parseAuthorAndDescription :: APT.Parser (String, Maybe UTCTime,String)
parseAuthorAndDescription = do
    APT.string "<i>"
    author <- APT.takeWhile (\c -> c /= ',')
    APT.take 2
    sDate <- APT.takeWhile (\c -> c /= '.')
    APT.skipWhile (\c -> c /= '>')
    APT.take 1
    APT.skipWhile (\c -> c /= '>')
    APT.take 1
    description <- APT.takeText
    return $ (unpack author, parseStupidTime . unpack $ sDate, unpack description)

-- "<i>Added by BryanOSullivan, Sat Jul  5 05:57:18 UTC 2014.</i><p>Pure and impure Bloom Filter implementations."

