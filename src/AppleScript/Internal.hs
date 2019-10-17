{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AppleScript.Internal
  ( createManyScript
  , listAllScript
  , deleteManyScript
  , updateManyScript
  , decodeRemindersList
  , listListsScript
  , convertBucket
  )
where

import qualified AppleScript.Types             as A
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
import           Data.Bifunctor                 ( first )
import           Data.Char                      ( toLower )
import qualified Data.Map.Strict               as MS
import           Data.Text                      ( splitOn
                                                , strip
                                                )
import           Types                          ( Reminders
                                                , Reminder(..)
                                                , TodoStatus(..)
                                                , BucketId
                                                , Bucket(..)
                                                , OrgLike(..)
                                                )
import           Universum

convertBucket :: A.ReminderList -> Bucket
convertBucket (A.ReminderList bid name) = Bucket bid name

instance OrgLike A.Reminder where
  from A.Reminder{..} = Reminder todoName todoId todoBody (Just status)
    where status = if todoCompleted then Done else InProgress

  to Reminder{..} = A.Reminder todoId
                       todoBody
                       (Just Done == todoStatus)
                       todoName
                       0
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing

instance FromJSON A.Reminder where
  parseJSON = withObject "Todo" $ \v -> A.Reminder
    <$> v .: "id"
    <*> v .:? "body"
    <*> v .: "completed"
    <*> v .: "name"
    <*> v .: "priority"
    <*> v .:? "dueDate"
    <*> v .:? "modificationDate"
    <*> v .:? "creationDate"
    <*> v .:? "completionDate"
    <*> v .:? "remindMeDate"

instance FromJSON A.ReminderList where
  parseJSON = withObject "TodoList" $ \l -> A.ReminderList
    <$> l .: "id"
    <*> l .: "name"

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

createManyScript :: BucketId -> [A.Reminder] -> LByteString
createManyScript _ rems = header
  <> mconcat (fmap (\n -> mconcat [st, encode n, en]) rems)
 where
  st :: LByteString
  st = "app.defaultList.reminders.push(app.Reminder("
  en :: LByteString
  en = "));"
  header :: LByteString
  header = "app = Application(\"Reminders\"); \n"

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

listListsScript :: LByteString
listListsScript
  = "app = Application('Reminders'); \n\
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


decodeRemindersList :: LByteString -> Either Text Reminders
decodeRemindersList t = do
  decoded <- first fromString (eitherDecode t)
  let names = A.todoName <$> decoded
      rems  = make . splitOn "|" <$> names

  return rems
 where
  make (name : id' : _) = Reminder (strip name) id' (Just "a") (pure Todo) -- TODO
  make _                = error "should not happen"
