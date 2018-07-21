module Main where

import           AppleScript         (runAppleScript)
import           Data.Semigroup      ((<>))
import           Lib
import           Options.Applicative

data Action = Add String
  deriving (Show)

addParser :: Parser Action
addParser = Add <$> argument str (metavar "REMINDER")

data Args = Args
  { action :: Action
  }

actionParser :: Parser Args
actionParser =
  Args <$>
  hsubparser (command "add" (info addParser (progDesc "Create new reminder")))

main :: IO ()
main = handle =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      ( fullDesc
     <> progDesc "Add new reminder"
     <> header "Remainders helper" )

handle :: Args -> IO ()
handle (Args (Add body)) = runAppleScript $ create reminder
  where
    reminder = Reminder body
--handle _             = return ()
