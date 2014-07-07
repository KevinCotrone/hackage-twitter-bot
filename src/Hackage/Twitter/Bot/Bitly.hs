{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hackage.Twitter.Bot.Bitly where


import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           GHC.Generics
import           Hackage.Twitter.Bot.Types
import qualified Network.Wreq              as W


-- Used as a placeholder for the return type of the bitly api
data BitLyData a = BitLyData {bitlyData :: a} deriving (Show)
-- API call to the short url
data BitLyURL = BitLyURL {bitLyUrl :: String} deriving (Show)
-- API Key for bitly
data BitLyKey = BitLyKey {bitLyKey :: String, lastErrorTime :: String} deriving (Show, Eq, Generic)


-- Instances to handle the parsing from bitly. Definitely not complete.
instance (FromJSON a) => FromJSON (BitLyData a) where
  parseJSON (Object o) = BitLyData <$> o .: "data"
  parseJSON _ = mzero

instance FromJSON BitLyKey where

instance FromJSON BitLyURL where
  parseJSON (Object o) = BitLyURL <$> o .: "url"
  parseJSON _ = mzero


-- Takes a post and attempts to shorten the url
shortenURLs :: BitLyKey -> FullPost -> IO (Maybe FullPost)
shortenURLs k p = do
  link' <- shortenURL k (fullPostLink p)
  case link' of
    (Just l) -> return . Just $ (p {fullPostLink = l})
    Nothing -> return Nothing

-- Shortens a URL string
shortenURL :: BitLyKey -> String -> IO (Maybe String)
shortenURL key longURL = do
  resp <- W.asJSON =<< W.get ("https://api-ssl.bitly.com" ++ "/v3/shorten?access_token=" ++ (bitLyKey key)  ++ "&longUrl=" ++ longURL )
  return $ bitLyUrl . bitlyData <$> resp ^? W.responseBody
