{-# LANGUAGE QuasiQuotes #-}

module AppleScript
  (runAppleScript
  ) where

import           Command                 (Command, CommandF (..), Reminder (..),
                                          name)
import           Control.Monad.Free
import           Control.Monad.IO.Class  (MonadIO)
import           Data.ByteString.Lazy    (toStrict)
import           Data.String.Interpolate (i)
import           Data.Text               (Text, split, strip)
import           Data.Text.Encoding      (decodeUtf8)
import           System.Process.Typed    (proc, readProcessStdout_)

executeAppleScript :: MonadIO m => String -> m Text
executeAppleScript script = do
  outBS <- readProcessStdout_ $ proc "/usr/bin/osascript" args
  return $ decodeUtf8 (toStrict outBS)
  where
    args = "-l" : "JavaScript" : "-e" : script : []

createTask :: MonadIO m => Text -> m ()
createTask taskName = (executeAppleScript script) >> return ()
  where
    script =
      [i|app = Application("Reminders")
         app.defaultList
           .reminders
           .push(app.Reminder({
             "name":"#{taskName}",
             "body":"asd222",
             "completed":false,
             "priority":9})) |]

listTasks :: MonadIO m => m [Reminder]
listTasks = do
  out <- executeAppleScript script
  return $ reminderFromText <$> split (== ',') out
  where
    script =
      [i| var r = Application('Reminders')
          var rems = [].slice.call(r.defaultList.reminders)
          rems.map(reminder => reminder.name())|]
    reminderFromText = Reminder . strip

runAppleScript :: Command x -> IO x
runAppleScript (Pure r)            = return r
runAppleScript (Free (All f))      = listTasks >>= runAppleScript . f
runAppleScript (Free (Create r x)) = (createTask (name r)) >> runAppleScript x
