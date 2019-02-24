module Main where

import           AppleScript         (runAppleScript)
import           Data.Semigroup      ((<>))
import           Lib
import           Options.Applicative
import           Parser              (runParser, allTitles, text)
import           Data.Text           (pack)
import           Data.Bifunctor      (bimap)
import           Data.Foldable       (fold, traverse_)

data Action =
  -- add one reminder item using Reminders Mac OS app
  Add String
  -- synchronize all off the items from file
  | Sync String
  deriving (Show)

addParser :: Parser Action
addParser = Add <$> argument str (metavar "REMINDER")

syncParser :: Parser Action
syncParser = Sync <$> argument str (metavar "FILEPATH")

data Args = Args
  { action :: Action
  , quiet :: Bool
  }

actionParser :: Parser Args
actionParser =
  Args
  <$> hsubparser (
  command "add" (info addParser (progDesc "Create new reminder"))
    <> command "sync" (info syncParser (progDesc "Synchronize items from spcified file"))
  )
  <*> switch (long "quiet" <> short 'q' <> help "testing")
  

main :: IO ()
main = handle =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      ( fullDesc
     <> progDesc "Add new reminder"
     <> header "Reminders helper" )

handle :: Args -> IO ()
handle (Args (Add body) False) = runAppleScript $ create reminder
  where
    reminder = Reminder (pack body)

handle (Args (Sync path) _) =
  let
    makeCommands = fmap (create . Reminder . text) . allTitles
  in
    runParser . pack <$> readFile path
    >>= fold . bimap print (traverse_ runAppleScript . makeCommands)

handle (Args _ True) = pack <$> readFile "test.org" >>= print . runParser
--handle _             = return ()
