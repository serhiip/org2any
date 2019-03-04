module Types
  ( Reminder (..)
  , Reminders
  ) where

import qualified Data.Text as T


data Reminder = Reminder { name :: T.Text }
  deriving (Show, Eq)

type Reminders = [Reminder]

