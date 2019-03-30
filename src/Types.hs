module Types
  ( Reminder (..)
  , Reminders
  , TodoStatus (..)
  ) where

import           Data.Set
import qualified Data.Text as T

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
  r == r2 = todoId r == todoId r2

instance Ord Reminder where
  compare r1 r2 = compare (todoId r1) (todoId r2)

type Reminders = Set Reminder
