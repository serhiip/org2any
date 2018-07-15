{-# LANGUAGE QuasiQuotes #-}

module AppleScript
  (createTask, listTasks) where

import           Control.Monad.IO.Class  (MonadIO)
import           Data.ByteString.Lazy    (toStrict)
import           Data.String.Interpolate (i)
import           Data.Text               (Text, split, unpack, strip)
import           Data.Text.Encoding      (decodeUtf8)
import           Lib                     (Reminder (Reminder))
import           System.Process.Typed    (proc, readProcessStdout_)


runAppleScript :: MonadIO m => String -> m Text
runAppleScript script = do
  outBS <- readProcessStdout_ $ proc "/usr/bin/osascript" args
  return $ decodeUtf8 (toStrict outBS)
  where
    args = "-l" : "JavaScript" : "-e" : script : []

createTask :: MonadIO m => String -> m ()
createTask name = (runAppleScript script) >> return ()
  where
    script =
      [i|app = Application("Reminders")
         app.defaultList
           .reminders
           .push(app.Reminder({
             "name":"#{name}",
             "body":"asd222",
             "completed":false,
             "priority":9})) |]

listTasks :: MonadIO m => m [Reminder]
listTasks = do
  out <- runAppleScript script
  return $ reminderFromText <$> split (== ',') out
  where
    script =
      [i| var r = Application('Reminders')
          var rems = [].slice.call(r.defaultList.reminders)
          rems.map(reminder => reminder.name())|]
    reminderFromText = Reminder . unpack . strip
