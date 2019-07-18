module Types
  ( Reminder(..)
  , Reminders
  , TodoStatus(..)
  , remindersFromList
  )
where

import           Data.Function                  ( on )
import qualified Data.Text                     as T
import           Data.Set                       ( fromList
                                                , Set
                                                )

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
