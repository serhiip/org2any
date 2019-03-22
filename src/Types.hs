module Types
  ( Reminder (..)
  , Reminders
  ) where

import qualified Data.Text as T

-- | Main representation of TODO item
data Reminder = Reminder
  { todoName :: T.Text
  , todoId   :: T.Text
  } deriving (Show)

instance Eq Reminder where
  r == r2 = todoId r == todoId r2

type Reminders = [Reminder]

