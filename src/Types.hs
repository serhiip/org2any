{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types
  ( Reminder(..)
  , Reminders
  , TodoStatus(..)
  , remindersFromList
  , remindersToMapping
  , O2AM(..)
  , runO2AM
  , SyncConfig(..)
  , Verbosity(..)
  )
where

import           Universum

import           Data.Function                  ( on )
import qualified Data.Text                     as T
import           Data.Set                       ( fromList
                                                , Set
                                                )
import qualified Data.Map.Strict               as MS

data Verbosity = Normal | Verbose

data SyncConfig = SyncConfig
      { configVorbose :: Verbosity
      , configThreadPerEvent :: Bool
      }

data TodoStatus
  = Todo
  | Done
  | InProgress
  deriving (Show, Eq, Ord)

-- | Main representation of TODO item
data Reminder = Reminder
  { todoName   :: T.Text
  , todoId     :: T.Text
  , todoBody   :: T.Text
  , todoStatus :: Maybe TodoStatus
  } deriving (Show)

instance Eq Reminder where
  (==) = (==) `on` todoId

instance Ord Reminder where
  compare = compare `on` todoId

type Reminders = Set Reminder

remindersFromList :: [Reminder] -> Reminders
remindersFromList = fromList

remindersToMapping :: Reminders -> MS.Map T.Text Reminder
remindersToMapping rems = MS.fromList $ (,) <$> todoId <*> id <$> toList @(Set Reminder) rems

newtype O2AM a = O2AM
  { getO2AM :: ReaderT SyncConfig IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader SyncConfig)

runO2AM :: MonadIO m => SyncConfig -> O2AM a -> m a
runO2AM config a = liftIO $ runReaderT (getO2AM a) config
