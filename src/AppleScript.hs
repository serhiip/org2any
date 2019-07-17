{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AppleScript
  ( runAppleScript
  )
where

import           Command                        ( Command
                                                , CommandF(..)
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Free
import           Control.Monad.IO.Class         ( MonadIO )
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
import           Data.Foldable                  ( toList )
import           Data.Text                      ( Text
                                                , split
                                                , splitOn
                                                , strip
                                                , unpack
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import           System.Process.Typed           ( proc
                                                , readProcessStdout_
                                                )
import           Types                          ( Reminder(..)
                                                , Reminders
                                                , TodoStatus(..)
                                                , remindersFromList
                                                )

instance ToJSON Reminder where
  toJSON (Reminder n id' b s) = object ["name" .= name, "body" .= b, "completed" .= status]
    where name = n <> " |" <> id'
          status = Done `elem` s

  toEncoding (Reminder n id' b s) = pairs ("name" .= name <> "body" .= b <> "completed" .= status)
    where name = n <> " |" <> id'
          status = Done `elem` s

execute :: MonadIO m => String -> m Text
execute script = do
  outBS <- readProcessStdout_ $ proc "/usr/bin/osascript" args
  return $ out outBS
 where
  args = ["-l", "JavaScript", "-e", script]
  out  = decodeUtf8 . toStrict

createMany :: MonadIO m => Reminders -> m ()
createMany rems = void . execute $ "app = Application(\"Reminders\"); " <> addAllScript
 where
  addAllScript =
    concatMap (\n -> "app.defaultList.reminders.push(app.Reminder(" <> (encodeUtf8 . encode) n <> "));") rems

list :: MonadIO m => m Reminders
list = do
  out <-
    execute
      "var r = Application('Reminders'); \
            \var rems = [].slice.call(r.defaultList.reminders); \
            \rems.map(reminder => reminder.name())"
  return
    $   remindersFromList
    .   fmap make
    $   Prelude.filter (\it -> length it > 1)
    $   splitOn "|"
    .   strip
    <$> Data.Text.split (== ',') out
 where
  make item = case item of
    (name : id' : _) -> Reminder name id' "a" (pure Todo)
    _                -> error "should not happen"

deleteMany :: MonadIO m => Reminders -> m ()
deleteMany rs =
  void
    . execute
    $ "app = Application('Reminders'); \
                               \items = app.defaultList.reminders(); \
                               \const deletions = "
    <> asJSObject rs
    <> "; \
                               \for (var item of items) { \
                               \  const [name, id] = item.name().split('|', 2); \
                               \  if (id in deletions) { \
                               \    app.delete(item); \
                               \  } \
                               \}"

updateMany :: MonadIO m => Reminders -> m ()
updateMany rs =
  void
    . execute
    $ "app = Application('Reminders'); \
                   \items = app.defaultList.reminders(); \
                   \updates = "
    <> asJSObject rs
    <> "; \
                   \for (const item of items) { \
                   \  const [name, id] = item.name().split('|', 2); \
                   \  if (id in updates) { \
                   \    const to = updates[id]; \
                   \    for (const attr_name in to) { \
                   \      const upd = to[attr_name]; \
                   \      if (item[attr_name]() != upd) { \
                   \        item[attr_name] = upd \
                   \      } \
                   \    } \
                   \  } \
                   \}"

encodeUtf8 :: ByteString -> String
encodeUtf8 = unpack . decodeUtf8 . toStrict

asJSObject :: Foldable m => m Reminder -> String
asJSObject rems = encodeUtf8 . encode . MS.fromList $ (,) <$> todoId <*> id <$> toList rems

runAppleScript :: Command x -> IO x
runAppleScript (Pure r                ) = return r
runAppleScript (Free (All f          )) = list >>= runAppleScript . f
runAppleScript (Free (CreateMany rs x)) = createMany rs >> runAppleScript x
runAppleScript (Free (DeleteMany rs x)) = deleteMany rs >> runAppleScript x
runAppleScript (Free (UpdateAll  rs x)) = updateMany rs >> runAppleScript x
