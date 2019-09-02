{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AppleScript.Internal
  ( createManyScript
  , listAllScript
  , deleteManyScript
  , updateManyScript
  , decodeRemindersList
  , convert
  )
where

import           Universum
import           Types                          ( Reminders
                                                , Reminder(..)
                                                , TodoStatus(..)
                                                , remindersFromList
                                                )
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                , (.:)
                                                , (.:?)
                                                , encode
                                                , withObject
                                                , eitherDecode
                                                , genericToEncoding
                                                , defaultOptions
                                                , fieldLabelModifier
                                                )

import           Data.Text                      ( splitOn
                                                , strip
                                                )
import           Data.Char                      ( toLower )
import qualified Data.Map.Strict               as MS
import qualified AppleScript.Types             as A

convert :: Reminder -> A.Reminder
convert r =
  A.Reminder (todoId r) (todoBody r) (Just Done == todoStatus r) (todoName r) 0 Nothing Nothing Nothing Nothing Nothing

instance FromJSON A.Reminder where
  parseJSON = withObject "Todo" $ \v -> A.Reminder
    <$> v .: "id"
    <*> v .: "body"
    <*> v .: "completed"
    <*> v .: "name"
    <*> v .: "priority"
    <*> v .:? "dueDate"
    <*> v .:? "modificationDate"
    <*> v .:? "creationDate"
    <*> v .:? "completionDate"
    <*> v .:? "remindMeDate"

instance ToJSON A.Reminder where
  toEncoding r = enc r { A.todoName = A.todoName r <> " |" <> A.todoId r}
    where
      enc =
            genericToEncoding defaultOptions
            { fieldLabelModifier =
              let
                firstToLower (f:rest) = toLower f : rest
                firstToLower [] = []
              in firstToLower . drop (length @String "todo")
            }

asJSObject :: [A.Reminder] -> LByteString
asJSObject = encode . MS.fromList . fmap ((,) <$> A.todoId <*> id)

createManyScript :: [A.Reminder] -> LByteString
createManyScript rems = header <> mconcat (fmap (\n -> mconcat [st, encode n, en]) rems)
 where
  st :: LByteString
  st = "app.defaultList.reminders.push(app.Reminder("
  en :: LByteString
  en = "));"
  header :: LByteString
  header = "app = Application(\"Reminders\"); \n"

listAllScript :: LByteString
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

deleteManyScript :: [A.Reminder] -> LByteString
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

updateManyScript :: [A.Reminder] -> LByteString
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
                         \if (upd && attr_name != 'id' && item[attr_name]() != upd) { \n\
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
