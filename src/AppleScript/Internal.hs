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
                                                , FromJSON(..)
                                                , object
                                                , pairs
                                                , (.=)
                                                , (.:)
                                                , (.:?)
                                                , encode
                                                , withObject
                                                , eitherDecode
                                                )

import           Data.Text                      ( splitOn
                                                , strip
                                                , unpack
                                                )

import qualified AppleScript.Types             as A

instance ToJSON Reminder where
  toJSON (Reminder n id' b s) = object ["name" .= name, "body" .= b, "completed" .= status]
    where name = n <> " |" <> id'
          status = Just Done == s

  toEncoding (Reminder n id' b s) = pairs ("name" .= name <> "body" .= b <> "completed" .= status)
    where name = n <> " |" <> id'
          status = Just Done == s

instance FromJSON A.Reminder where
  parseJSON = withObject "Todo" $ \v -> A.Reminder
    <$> v .:? "dueDate"
    <*> v .:? "modificationDate"
    <*> v .: "creationDate"
    <*> v .:? "completionDate"
    <*> v .:? "remindMeDate"
    <*> v .: "body"
    <*> v .: "completed"
    <*> v .: "id"
    <*> v .: "name"
    <*> v .: "priority"

asStr :: LByteString -> String
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
                \var rems = r.defaultList.reminders(); \n\
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

decodeRemindersList :: LByteString -> Either String Reminders
decodeRemindersList t = do
  decoded <- eitherDecode t
  let names = A.todoName <$> decoded
      rems  = make . splitOn "|" <$> names

  return $ remindersFromList rems
 where
  make (name : id' : _) = Reminder (strip name) id' "a" (pure Todo)
  make _                = error "should not happen"
