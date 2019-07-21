{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AppleScript.Internal
  ( createManyScript
  , listAllScript
  , deleteManyScript
  , updateManyScript
  , decodeRemindersList
  )
where

import           Universum
import           Types                          ( Reminders
                                                , Reminder(..)
                                                , TodoStatus(..)
                                                , remindersFromList
                                                , remindersToMapping
                                                )
import           Data.Aeson                     ( ToJSON(..)
                                                , object
                                                , pairs
                                                , (.=)
                                                , encode
                                                )

import           Data.Text                      ( Text
                                                , split
                                                , splitOn
                                                , strip
                                                , unpack
                                                )

import qualified Data.ByteString.Lazy          as BSL


instance ToJSON Reminder where
  toJSON (Reminder n id' b s) = object ["name" .= name, "body" .= b, "completed" .= status]
    where name = n <> " |" <> id'
          status = maybe False (== Done) s

  toEncoding (Reminder n id' b s) = pairs ("name" .= name <> "body" .= b <> "completed" .= status)
    where name = n <> " |" <> id'
          status = maybe False (== Done) s

asStr :: BSL.ByteString -> String
asStr = unpack . decodeUtf8

asJSObject :: Reminders -> String
asJSObject = asStr . encode . remindersToMapping

createManyScript :: Reminders -> String
createManyScript rems =
  "app = Application(\"Reminders\"); \n"
    <> concatMap (\n -> "app.defaultList.reminders.push(app.Reminder(" <> (asStr . encode) n <> "));") rems

listAllScript :: String
listAllScript
  = "var r = Application('Reminders'); \n\
                \var rems = [].slice.call(r.defaultList.reminders); \n\
                \rems.map(reminder => reminder.name())"

deleteManyScript :: Reminders -> String
deleteManyScript reminders =
  "app = Application('Reminders'); \n\
                               \items = app.defaultList.reminders(); \n\
                               \const deletions = "
    <> asJSObject reminders
    <> "; \n\
                               \for (var item of items) { \n\
                                 \const [name, id] = item.name().split('|', 2); \n\
                                 \if (id in deletions) { \n\
                                    \app.delete(item); \n\
                                 \} \n\
                               \}"

updateManyScript :: Reminders -> String
updateManyScript reminders =
  "app = Application('Reminders'); \n\
                   \items = app.defaultList.reminders(); \n\
                   \updates = "
    <> asJSObject reminders
    <> "; \n\
                   \for (const item of items) { \n\
                     \const [name, id] = item.name().split('|', 2); \n\
                     \if (id in updates) { \n\
                       \const to = updates[id]; \n\
                       \for (const attr_name in to) { \n\
                         \const upd = to[attr_name]; \n\
                         \if (item[attr_name]() != upd) { \n\
                           \item[attr_name] = upd \n\
                         \} \n\
                       \} \n\
                     \} \n\
                   \}"


decodeRemindersList :: Text -> Reminders
decodeRemindersList t =
  remindersFromList . fmap make $ filter (\it -> length it > 1) $ splitOn "|" . strip <$> Data.Text.split (== ',') t
 where
  make item = case item of
    (name : id' : _) -> Reminder name id' "a" (pure Todo)
    _                -> error "should not happen"
