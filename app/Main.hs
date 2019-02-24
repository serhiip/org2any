module Main where

import           AppleScript         (runAppleScript)
import           Command
import           Data.Bifunctor      (bimap)
import           Data.Foldable       (fold, traverse_)
import           Data.Semigroup      ((<>))
import           Data.Text           (pack)
import           Options.Applicative
import           Parser              (allTitles, runParser, text)

import           System.Directory
import           System.FilePath
import           System.FSNotify     hiding (Action)

data Action
  = Add { reminderText :: String }
  | Sync { filePath :: FilePath
         , watch    :: Bool }
  deriving (Show)

watchOpt :: Parser Bool
watchOpt =
  switch
    (long "watch" <> short 'w' <>
     help "Watch file changes and execute a sync on each change")

fileArg :: Parser FilePath
fileArg = argument str (metavar "FILEPATH")

addParser :: Parser Action
addParser = Add <$> argument str (metavar "REMINDER")

syncParser :: Parser Action
syncParser = Sync <$> fileArg <*> watchOpt

data Args = Args
  { action :: Action
  }

actionParser :: Parser Args
actionParser =
  Args <$>
  hsubparser
    (command "add" (info addParser (progDesc "Create new reminder")) <>
     command
       "sync"
       (info syncParser (progDesc "Synchronize items from spcified file")))

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
