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

import           Types                          ( Reminders
                                                , Reminder(..)
                                                , TodoStatus(..)
                                                , remindersFromList
                                                )
import           Data.Aeson                     ( ToJSON(..)
                                                , object
                                                , pairs
                                                , (.=)
                                                , encode
                                                )

import           Data.ByteString.Lazy           ( ByteString
                                                , toStrict
                                                )

import qualified Data.Map.Strict               as MS
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Text                      ( Text
                                                , split
                                                , splitOn
                                                , strip
                                                , unpack
                                                )
import           Data.Foldable                  ( toList )



instance ToJSON Reminder where
  toJSON (Reminder n id' b s) = object ["name" .= name, "body" .= b, "completed" .= status]
    where name = n <> " |" <> id'
          status = Done `elem` s

  toEncoding (Reminder n id' b s) = pairs ("name" .= name <> "body" .= b <> "completed" .= status)
    where name = n <> " |" <> id'
          status = Done `elem` s

encodeUtf8 :: ByteString -> String
encodeUtf8 = unpack . decodeUtf8 . toStrict

asJSObject :: Foldable m => m Reminder -> String
asJSObject rems = encodeUtf8 . encode . MS.fromList $ (,) <$> todoId <*> id <$> toList rems

createManyScript :: Reminders -> String
createManyScript rems =
  "app = Application(\"Reminders\"); \n"
    <> concatMap (\n -> "app.defaultList.reminders.push(app.Reminder(" <> (encodeUtf8 . encode) n <> "));") rems

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
  remindersFromList
    .   fmap make
    $   Prelude.filter (\it -> length it > 1)
    $   splitOn "|"
    .   strip
    <$> Data.Text.split (== ',') t
 where
  make item = case item of
    (name : id' : _) -> Reminder name id' "a" (pure Todo)
    _                -> error "should not happen"
