{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AppleScript
  (runAppleScript
  ) where

import           Command                 (Command, CommandF (..))
import           Control.Monad           (void)
import           Control.Monad.Free
import           Control.Monad.IO.Class  (MonadIO)
import           Data.ByteString.Lazy    (toStrict)
import           Data.String.Interpolate (i)
import           Data.Text               (Text, split, splitOn, strip)
import           Data.Text.Encoding      (decodeUtf8)
import           System.Process.Typed    (proc, readProcessStdout_)
import           Types                   (Reminder (..))

execute :: MonadIO m => String -> m Text
execute script = do
  outBS <- readProcessStdout_ $ proc "/usr/bin/osascript" args
  return $ out outBS
  where
    args = ["-l", "JavaScript", "-e", script]
    out = decodeUtf8 . toStrict

create :: MonadIO m => Reminder -> m ()
create = createAll . pure

createAll :: MonadIO m => [Reminder] -> m ()
createAll names =
  void $ execute [i|app = Application("Reminders"); #{addAllScript}|]
  where
    addAllScript =
      concatMap
        (\n ->
           [i|app.defaultList.reminders.push(
               app.Reminder({
                 "name": "#{todoName n} |#{todoId n}",
                 "body": "asd222",
                 "completed":false,
                 "priority":9}));|])
        names

list :: MonadIO m => m [Reminder]
list = do
  out <-
    execute
      [i| var r = Application('Reminders')
          var rems = [].slice.call(r.defaultList.reminders)
          rems.map(reminder => reminder.name())|]
  return $
    fmap make $
    filter (\it -> length it > 1) $ splitOn "|" . strip <$> split (== ',') out
  where
    make item =
      case item of
        (name:id':_) -> Reminder name id'
        _ -> error "should not happen"

del :: MonadIO m => Reminder -> m ()
del r = void $ execute
           [i| app = Application('Reminders')
               items = app.defaultList.reminders()
               for (var item of items) {
                   if (item.name() == '#{todoName r}') {
                       app.delete(item)
                   }
               }|]

runAppleScript :: Command x -> IO x
runAppleScript (Pure r)                 = return r
runAppleScript (Free (All f))           = list >>= runAppleScript . f
runAppleScript (Free (Create r x))      = create r >> runAppleScript x
runAppleScript (Free (CreateMany rs x)) = createAll rs >> runAppleScript x
runAppleScript (Free (Delete r x))      = del r >> runAppleScript x
