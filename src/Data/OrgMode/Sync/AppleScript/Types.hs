{-|
Module      : Data.OrgMode.Sync.AppleScript.Types
Description : Types related to OS X Reminders interaction
License     : GPL-3
Maintainer  : Serhii <serhii@proximala.bz>
Stability   : experimental
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.OrgMode.Sync.AppleScript.Types
  ( Reminder(..)
  , ReminderList(..)
  )
where


import           Data.Time.LocalTime            ( ZonedTime )
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                , (.:)
                                                , (.:?)
                                                , withObject
                                                , genericToEncoding
                                                , defaultOptions
                                                , fieldLabelModifier
                                                )
import           Data.Char                      ( toLower )
import qualified Data.OrgMode.Sync.Types       as O
import Universum
import           Data.Text                      ( splitOn
                                                , strip
                                                )


-- | A Reminders OS X app representation of TODO list
data ReminderList = ReminderList {
    listId :: Text
  , listName :: Text
  } deriving (Show, Eq, Ord, Generic)

-- | Item representation of Reminders OS X application.
data Reminder = Reminder {
    todoId :: Text
    -- ^ Unique identifier
  , todoBody :: Maybe Text
    -- ^ Reminder text
  , todoCompleted :: Bool
    -- ^ Completion state
  , todoName :: Text
    -- ^ Reminder title, current implementation requires this field to
    -- contain the internal ID delimited from title with pipe character
  , todoPriority :: Int
    -- ^ Reminder priority number
  , todoDueDate :: Maybe ZonedTime
  , todoModificationDate :: Maybe ZonedTime
  , todoCreationDate :: Maybe ZonedTime
  , todoCompletionDate :: Maybe ZonedTime
  , todoRemindMeDate :: Maybe ZonedTime
  } deriving (Show, Generic)

instance FromJSON Reminder where
  parseJSON = withObject "Todo" $ \value ->
    Reminder
      <$> (value .: "id")
      <*> (value .:? "body")
      <*> (value .: "completed")
      <*> (value .: "name")
      <*> (value .: "priority")
      <*> (value .:? "dueDate")
      <*> (value .:? "modificationDate")
      <*> (value .:? "creationDate")
      <*> (value .:? "completionDate")
      <*> (value .:? "remindMeDate")

instance FromJSON ReminderList where
  parseJSON = withObject "TodoList" $ \l -> ReminderList <$> l .: "id" <*> l .: "name"

instance ToJSON Reminder where
  toEncoding r = enc r { todoName = todoName r <> " |" <> todoId r }
   where
    enc = genericToEncoding defaultOptions
      { fieldLabelModifier = let firstToLower (f : rest) = toLower f : rest
                                 firstToLower []         = []
                             in  firstToLower . drop (length @String "todo")
      }

instance O.OrgLike Reminder where
  from Reminder {..} = make . splitOn "|" $ todoName
   where
    status = if todoCompleted then O.Done else O.InProgress
    make (name : orgId : _) =
      pure $ O.Reminder (strip name) orgId todoBody (pure status) todoId
    make _ = mzero

  to O.Reminder {..} = Reminder todoId
                                todoBody
                                (Just O.Done == todoStatus)
                                todoName
                                0
                                Nothing
                                Nothing
                                Nothing
                                Nothing
                                Nothing


