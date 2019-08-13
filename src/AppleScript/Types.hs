{-# LANGUAGE DeriveGeneric #-}

module AppleScript.Types
  ( Reminder(..)
  )
where

import           GHC.Generics

import           Data.Time.LocalTime            ( ZonedTime )
import           Universum

data Reminder = Reminder { todoDueDate :: Maybe ZonedTime
                 , todoModificationDate :: Maybe ZonedTime
                 , todoCreationDate :: Maybe ZonedTime
                 , todoCompletionDate :: Maybe ZonedTime
                 , todoRemindMeDate :: Maybe ZonedTime
                 , todoBody :: Text
                 , todoCompleted :: Bool
                 , todoId :: Text
                 , todoName :: Text
                 , todoPriority :: Int
                 } deriving (Show, Generic)
