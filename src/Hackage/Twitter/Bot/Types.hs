module Hackage.Twitter.Bot.Types where

import           Data.Time       ()
import           Data.Time.Clock

data FullPost = FullPost { fullPostAuthor :: String, fullPostDescription :: String, fullPostTime :: UTCTime, fullPostTitle :: String, fullPostLink :: String} deriving (Show)

data PartialPost = PartialPost {author :: String , description :: String, postTime :: UTCTime} deriving (Show)
