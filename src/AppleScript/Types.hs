{-# LANGUAGE DeriveGeneric #-}

module AppleScript.Types
  ( Reminder(..)
  )
where

import           GHC.Generics

import           Data.Time.LocalTime            ( ZonedTime )
import           Universum

data Reminder = Reminder {
    todoId :: Text
  , todoBody :: Text
  , todoCompleted :: Bool
  , todoName :: Text
  , todoPriority :: Int
  , todoDueDate :: Maybe ZonedTime
  , todoModificationDate :: Maybe ZonedTime
  , todoCreationDate :: Maybe ZonedTime
  , todoCompletionDate :: Maybe ZonedTime
  , todoRemindMeDate :: Maybe ZonedTime
  } deriving (Show, Generic)
