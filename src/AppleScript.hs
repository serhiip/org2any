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
import           Data.ByteString.Lazy    (ByteString, toStrict)
import qualified Data.Map.Strict         as MS
import           Data.Set
import           Data.String.Interpolate (i)
import           Data.Text               (Text, split, splitOn, strip, unpack)
import           Data.Text.Encoding      (decodeUtf8)
import           System.Process.Typed    (proc, readProcessStdout_)
import           Types                   (Reminder (..), Reminders,
                                          TodoStatus (..))

instance ToJSON Reminder where
  toJSON (Reminder n id' b s) = object ["name" .= name, "body" .= b, "completed" .= status]
    where name = unpack n ++ " |" ++ unpack id'
          status = Done `elem` s


  toEncoding (Reminder n id' b s) = pairs ("name" .= name <> "body" .= b <> "completed" .= status)
    where name = unpack n ++ " |" ++ unpack id'
          status = Done `elem` s

execute :: MonadIO m => String -> m Text
execute script = do
  outBS <- readProcessStdout_ $ proc "/usr/bin/osascript" args
  return $ out outBS
  where
    args = ["-l", "JavaScript", "-e", script]
    out = decodeUtf8 . toStrict

createMany :: MonadIO m => Reminders -> m ()
createMany rems =
  void $ execute [i|app = Application("Reminders"); #{addAllScript}|]
  where
    addAllScript =
      concatMap
        (\n -> [i|app.defaultList.reminders.push(app.Reminder(#{encode n}));|])
        rems

list :: MonadIO m => m Reminders
list = do
  out <-
    execute
      [i| var r = Application('Reminders')
          var rems = [].slice.call(r.defaultList.reminders)
          rems.map(reminder => reminder.name())|]
  return $
    fromList . fmap make $
    Prelude.filter (\it -> length it > 1) $
    splitOn "|" . strip <$> Data.Text.split (== ',') out
  where
    make item =
      case item of
        (name:id':_) -> Reminder name id' "a" (pure Todo)
        _            -> error "should not happen"

deleteMany :: MonadIO m => Reminders -> m ()
deleteMany rs = void $ execute
           [i| app = Application('Reminders')
               items = app.defaultList.reminders()
               const deletions = #{asJSObject rs}
               for (var item of items) {
                   const [name, id] = item.name().split('|', 2)
                   if (id in deletions) {
                       app.delete(item)
                   }
               }|]

updateMany :: MonadIO m => Reminders -> m ()
updateMany rems =
  void $ execute [i|
                   app = Application('Reminders')
                   items = app.defaultList.reminders()
                   updates = #{asJSObject rems}
                   for (const item of items) {
                       const [name, id] = item.name().split('|', 2)
                       if (id in updates) {
                           const to = updates[id]
                           for (const attr_name in to) {
                               const upd = to[attr_name]
                               if (item[attr_name]() != upd) {
                                   item[attr_name] = upd
                               }
                           }
                        }
                    }|]

asJSObject :: Reminders -> ByteString
asJSObject rems =
  encode . MS.fromList $ (,) <$> (unpack . todoId) <*> id <$> toList rems

runAppleScript :: Command x -> IO x
runAppleScript (Pure r)                 = return r
runAppleScript (Free (All f))           = list >>= runAppleScript . f
runAppleScript (Free (CreateMany rs x)) = createMany rs >> runAppleScript x
runAppleScript (Free (DeleteMany rs x)) = deleteMany rs >> runAppleScript x
runAppleScript (Free (UpdateAll rs x))  = updateMany rs >> runAppleScript x
