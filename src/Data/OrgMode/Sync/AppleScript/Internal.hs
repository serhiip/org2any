{-|
Module      : Data.OrgMode.Sync.AppleScript.Internal
Description : Scripts to connect with Reminders OS X app
License     : GPL-3
Maintainer  : Serhii <serhii@proximala.bz>
Stability   : experimental

This scripts are written in
 <https://developer.apple.com/library/archive/documentation/LanguagesUtilities/Conceptual/MacAutomationScriptingGuide/index.html AppleScript>
 JavaScript dialect and used to synchronize stuff with
 Reminders OS X application.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.OrgMode.Sync.AppleScript.Internal
  ( createManyScript
  , listAllScript
  , deleteManyScript
  , updateManyScript
  , decodeOSXReminders
  , listListsScript
  , convertBucket
  )
where

import qualified Data.OrgMode.Sync.AppleScript.Types
                                               as A
import           Data.Aeson                     ( encode
                                                , eitherDecode
                                                )
import           Data.Bifunctor                 ( first )
import qualified Data.Map.Strict               as MS
import           Data.OrgMode.Sync.Types        ( BucketId
                                                , Bucket(..)
                                                )
import           Universum

-- | Make a few new reminders in some list
createManyScript :: BucketId -> [A.Reminder] -> LByteString
createManyScript _ rems = "app = Application(\"Reminders\"); \n" <> mconcat
  ((\n -> "app.defaultList.reminders.push(app.Reminder(" <> encode n <> "));") <$> rems)

-- | List all reminders in a list
listAllScript :: BucketId -> LByteString
listAllScript bid =
  "app = Application('Reminders'); \n\
  \list = app.lists().filter(_ => _.id() == '"
    <> encodeUtf8 @Text @LByteString bid
    <> "')[0]; \n\
  \var rems = list.reminders(); \n\
  \const res = rems.map(reminder => { \n\
    \const props = reminder.properties(); \n\
    \props['container'] = null; \n\
    \for (var property in props) \n\
      \if (props.hasOwnProperty(property)) \n\
        \if (props[property] && props[property].toISOString) \n\
          \props[property] = props[property].toISOString() \n\
      \return props \n\
    \}) \n\
  \JSON.stringify(res)"

-- | Delete some reminders from list by ID stored in a title
deleteManyScript :: BucketId -> [A.Reminder] -> LByteString
deleteManyScript bid reminders =
  "app = Application('Reminders'); \n\
  \list = app.lists().filter(_ => _.id() == '"
    <> encodeUtf8 @Text @LByteString bid
    <> "')[0];\n\
  \items = list.reminders(); \n\
  \const deletions = "
    <> asJSObject reminders
    <> "; \n\
  \for (var item of items) { \n\
    \const [name, id] = item.name().split('|', 2); \n\
    \if (id in deletions) { \n\
      \app.delete(item); \n\
    \} \n\
  \}"

-- | Update some items in a list by identifying them by id stored
-- in a title
updateManyScript :: BucketId -> [A.Reminder] -> LByteString
updateManyScript bid reminders =
  "app = Application('Reminders'); \n\
  \list = app.lists().filter(_ => _.id() == '"
    <> encodeUtf8 @Text @LByteString bid
    <> "')[0];\n\
  \items = list.reminders(); \n\
  \updates = "
    <> asJSObject reminders
    <> "; \n\
  \for (const item of items) { \n\
    \const [name, id] = item.name().split('|', 2); \n\
    \if (id in updates) { \n\
      \const to = updates[id]; \n\
      \for (const attr_name in to) { \n\
        \const upd = to[attr_name]; \n\
        \if (upd && attr_name != 'id' && item[attr_name]() != upd) { \n\
          \item[attr_name] = upd \n\
        \} \n\
      \} \n\
    \} \n\
  \}"

-- | Get a list of available reminder lists
listListsScript :: LByteString
listListsScript =
  "app = Application('Reminders'); \n\
  \const res = app.lists().map(reminder => { \n\
    \const props = reminder.properties(); \n\
    \props['container'] = null; \n\
    \for (var property in props) \n\
      \if (props.hasOwnProperty(property)) \n\
        \if (props[property] && props[property].toISOString) \n\
          \props[property] = props[property].toISOString(); \n\
    \return props; \n\
  \}); \n\
  \JSON.stringify(res);"

-- | Helper function to decode the list of reminders.
decodeOSXReminders :: LByteString -> Either Text [A.Reminder]
decodeOSXReminders = first toText . eitherDecode

-- | Convert Reminders app representation to generic one
convertBucket :: A.ReminderList -> Bucket
convertBucket (A.ReminderList bid name) = Bucket bid name

-- | Helper to make a JSON object
asJSObject :: [A.Reminder] -> LByteString
asJSObject = encode . MS.fromList . fmap ((,) <$> A.todoId <*> id)

