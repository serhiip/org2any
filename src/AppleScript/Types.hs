{-# LANGUAGE DeriveGeneric #-}

module AppleScript.Types
  ( Reminder(..)
  , ReminderList(..)
  )
where

import           GHC.Generics

import           Data.Time.LocalTime            ( ZonedTime )
import           Universum

data ReminderList = ReminderList {
    listId :: Text
  , listName :: Text
  } deriving (Show, Eq, Ord, Generic)

data Reminder = Reminder {
    todoId :: Text
  , todoBody :: Maybe Text
  , todoCompleted :: Bool
  , todoName :: Text
  , todoPriority :: Int
  , todoDueDate :: Maybe ZonedTime
  , todoModificationDate :: Maybe ZonedTime
  , todoCreationDate :: Maybe ZonedTime
  , todoCompletionDate :: Maybe ZonedTime
  , todoRemindMeDate :: Maybe ZonedTime
  } deriving (Show, Generic)
