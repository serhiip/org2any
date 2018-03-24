module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Action =
  Add { rawNotes :: String }
  deriving (Show)

addParser :: Parser Action
addParser = Add <$> argument str (metavar "REMINDERS")

data Args = Args
  { action :: Action }

actionParser :: Parser Args
actionParser =
  Args <$>
  hsubparser (command "add" (info addParser (progDesc "Create new notes")))

main :: IO ()
main = handle =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      ( fullDesc
     <> progDesc "Create or list remainders in osX Reminders app"
     <> header "Remainders helper" )

handle :: Args -> IO ()
handle (Args a) = putStrLn . show $ a
--handle _             = return ()
