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


module Hackage.Twitter.Bot.Acid where


import           Control.Applicative        ((<$>))
import           Control.Monad.Reader       (ask)
import           Control.Monad.State        (get, put)
import           Data.Acid                  (Query, Update, makeAcidic)
import           Data.Data                  (Data, Typeable)
import           Data.SafeCopy              (base, deriveSafeCopy)
import           Data.Time.Calendar
import           Data.Time.Clock

-- Keeping track of the last post time in an acid state

data LastTimeState = LastTimeState { lastTime :: UTCTime }
    deriving (Eq, Ord, Read, Show, Data, Typeable)


$(deriveSafeCopy 0 'base ''LastTimeState)

initialLastTimeState :: LastTimeState
initialLastTimeState =  LastTimeState $ UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)


-- Chooses the max time from the stored time or the given time and then stores it.

setNewLastTime :: UTCTime -> Update LastTimeState UTCTime
setNewLastTime time = do
  c@LastTimeState{..} <- get
  let time' = max time lastTime
  put $ LastTimeState time'
  return time'

-- A getter for the last time

peekLastTime :: Query LastTimeState UTCTime
peekLastTime = lastTime <$> ask


-- Make everything acidic
$(makeAcidic ''LastTimeState ['setNewLastTime, 'peekLastTime])
