{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Types
  ( Reminder(..)
  , Reminders
  , TodoStatus(..)
  , remindersFromList
  , remindersToMapping
  )
where

import Universum

import           Data.Function                  ( on )
import qualified Data.Text                     as T
import           Data.Set                       ( fromList
                                                , Set
                                                )
import qualified Data.Map.Strict               as MS

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
remindersToMapping rems =
  MS.fromList $ (,) <$> todoId <*> id <$> toList @(Set Reminder) rems
