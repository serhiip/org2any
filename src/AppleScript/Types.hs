module AppleScript.Types
  ( Reminder(..)
  )
where

import           Data.Time.LocalTime            ( ZonedTime )
import           Universum

data Reminder = Reminder { todoDueDate :: Maybe ZonedTime
                 , todoModificationDate :: Maybe ZonedTime
                 , todoCreationDate :: ZonedTime
                 , todoCompletionDate :: Maybe ZonedTime
                 , todoRemindMeDate :: Maybe ZonedTime
                 , todoBody :: Text
                 , todoCompleted :: Bool
                 , todoId :: Text
                 , todoName :: Text
                 , todoPriority :: Int
                 } deriving (Show)
