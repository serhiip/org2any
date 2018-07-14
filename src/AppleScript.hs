{-# LANGUAGE QuasiQuotes #-}

module AppleScript
  (createTask) where

import           Control.Monad.IO.Class  (MonadIO)
import           Data.String.Interpolate (i)
import           System.Process.Typed    (shell, runProcess_)


runAppleScript :: MonadIO m => String -> m ()
runAppleScript script = runProcess_ $ shell command
  where command = "osascript -l JavaScript -e " ++ script

createTask :: MonadIO m => String -> m ()
createTask name = runAppleScript script
  where script = [i|'
                   app = Application("Reminders")
                   app.defaultList
                     .reminders
                     .push(app.Reminder({
                       "name":"#{name}",
                       "body":"asd222",
                       "completed":false,
                       "priority":9})) '|]
