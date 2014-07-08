{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Common where

import Web.Twitter.Conduit

import Web.Authenticate.OAuth as OA
import qualified Network.URI as URI
import Network.HTTP.Conduit
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive as CI
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Trans.Resource
import System.Environment
import Control.Monad.Logger
import Control.Lens
import GHC.Generics
import Data.Yaml
import Data.Aeson


data TwitterAuthentication = TwitterAuthentication {
    consumerKey :: String
   ,consumerSecret :: String
   ,accessToken :: String
   ,accessSecret :: String
} deriving (Show, Read, Eq, Generic)

instance FromJSON TwitterAuthentication where

readOauthTokens :: String -> IO  (OAuth, Credential)
readOauthTokens conf = do
  mOauth <- decodeFile conf :: IO (Maybe TwitterAuthentication)
  case mOauth of
    (Just oauth) -> return $ ((twitterOAuth {oauthConsumerKey = S8.pack $ consumerKey oauth, oauthConsumerSecret = S8.pack $ consumerSecret oauth}), Credential [("oauth_token", S8.pack $  accessToken oauth),("oauth_token_secret", S8.pack $  accessSecret oauth)])
    Nothing -> fail "Error reading oauth credentials"

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
    accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
    accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
  where
    getEnv' = (S8.pack <$>) . getEnv

getProxyEnv :: IO (Maybe Proxy)
getProxyEnv = do
    env <- M.fromList . over (mapped . _1) CI.mk <$> getEnvironment
    let u = M.lookup "https_proxy" env <|>
            M.lookup "http_proxy" env <|>
            M.lookup "proxy" env >>= URI.parseURI >>= URI.uriAuthority
    return $ Proxy <$> (S8.pack . URI.uriRegName <$> u) <*> (parsePort . URI.uriPort <$> u)
  where
    parsePort :: String -> Int
    parsePort []       = 8080
    parsePort (':':xs) = read xs
    parsePort xs       = error $ "port number parse failed " ++ xs

runTwitterFromEnv :: (MonadIO m, MonadBaseControl IO m) => TW (ResourceT m) a -> m a
runTwitterFromEnv task = do
    pr <- liftBase getProxyEnv
    (oa, cred) <- liftBase getOAuthTokens
    let env = (setCredential oa cred def) { twProxy = pr }
    runTW env task

runTwitterFromEnv' :: (MonadIO m, MonadBaseControl IO m) => TW (ResourceT (NoLoggingT m)) a -> m a
runTwitterFromEnv' = runNoLoggingT . runTwitterFromEnv

runTwitter' :: (MonadIO m, MonadBaseControl IO m) => (OAuth, Credential) -> TW (ResourceT (NoLoggingT m)) a -> m a
runTwitter' oauth = runNoLoggingT . (runTwitter oauth)

runTwitter :: (MonadIO m, MonadBaseControl IO m) => (OAuth, Credential) -> TW (ResourceT m) a -> m a
runTwitter (oa, cred) task = do
    pr <- liftBase getProxyEnv
    let env = (setCredential oa cred def) { twProxy = pr }
    runTW env task