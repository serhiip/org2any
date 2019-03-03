module Args (
  actionParser
  , Args (..)
  , Action (..)
  , arguments
  , execParser
  ) where

import           Options.Applicative

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

arguments :: ParserInfo Args
arguments =
  info
    (actionParser <**> helper)
    (fullDesc <> progDesc "Add new reminder" <> header "Reminders helper")
