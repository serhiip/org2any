module Main where

import           AppleScript         (runAppleScript)
import           Command
import           Data.Bifunctor      (bimap)
import           Data.Foldable       (fold, traverse_)
import           Data.Semigroup      ((<>))
import           Data.Text           (pack)
import           Options.Applicative
import           Parser              (allTitles, runParser, text)

import           Args
import           System.Directory
import           System.FilePath
import           System.FSNotify     hiding (Action)

main :: IO ()
main = handle =<< execParser opts
  where
    opts =
      info
        (actionParser <**> helper)
        (fullDesc <> progDesc "Add new reminder" <> header "Reminders helper")

    handle :: Args -> IO ()
    handle (Args (Add body)) = runAppleScript $ create reminder
      where
        reminder = Reminder (pack body)
    handle (Args (Sync path toWatch)) =
      if toWatch
      then withManager $ \mgr -> do
        canonPath <- canonicalizePath path
        stop <-
          watchDir
          mgr
          (takeDirectory path)
          (equalFilePath canonPath . eventPath)
          (const execute)
        putStrLn "Listening for changes..."
        putStrLn "📝 Press any key to stop"
        _ <- getChar
        stop
      else execute
      where
        makeCommands = fmap (create . Reminder . text) . allTitles
        execute =
          runParser . pack <$> readFile path >>=
          fold . bimap print (traverse_ runAppleScript . makeCommands)
