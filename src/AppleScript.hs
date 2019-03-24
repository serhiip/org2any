{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AppleScript
  (runAppleScript
  ) where

import           Command                 (Command, CommandF (..))
import           Control.Monad           (void)
import           Control.Monad.Free
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Aeson
import           Data.ByteString.Lazy    (toStrict)
import           Data.Map.Strict         (fromList)
import           Data.String.Interpolate (i)
import           Data.Text               (Text, split, splitOn, strip, unpack)
import           Data.Text.Encoding      (decodeUtf8)
import           System.Process.Typed    (proc, readProcessStdout_)
import           Types                   (Reminder (..), Reminders)

instance ToJSON Reminder where
  toJSON (Reminder n id') = object ["name" .= name, "body" .= ("nn" :: String)]
    where name = unpack n ++ " |" ++ unpack id'

  toEncoding (Reminder n id') = pairs ("name" .= name <> "body" .= ("nn" :: String))
    where name = unpack n ++ " |" ++ unpack id'

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
        _            -> error "should not happen"

del :: MonadIO m => Reminder -> m ()
del r = void $ execute
           [i| app = Application('Reminders')
               items = app.defaultList.reminders()
               for (var item of items) {
                   if (item.name().indexOf('#{todoId r}') > -1) {
                       app.delete(item)
                   }
               }|]

updateAll :: MonadIO m => Reminders -> m ()
updateAll rems =
  void $ execute [i|
                   app = Application('Reminders')
                   items = app.defaultList.reminders()
                   updates = #{enc}
                   for (const item of items) {
                       const [name, id] = item.name().split('|', 2)
                       if (id in updates) {
                           const to = updates[id]
                           for (const attr_name in to) {
                               const upd = to[attr_name]
                               if (attr_name == 'name') {
                                   item['name'] = to['name'] + ' |' + id
                               } else {
                                   if (item[attr_name]() != upd) {
                                       item[attr_name] = upd
                                   }
                               }
                           }
                        }
                    }|]
  where enc = fromList $ (,) <$> unpack . todoId <*> encode <$> rems

runAppleScript :: Command x -> IO x
runAppleScript (Pure r)                 = return r
runAppleScript (Free (All f))           = list >>= runAppleScript . f
runAppleScript (Free (Create r x))      = create r >> runAppleScript x
runAppleScript (Free (CreateMany rs x)) = createAll rs >> runAppleScript x
runAppleScript (Free (Delete r x))      = del r >> runAppleScript x
runAppleScript (Free (UpdateAll rs x))  = updateAll rs >> runAppleScript x
