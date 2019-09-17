module Args
  ( Args(..)
  , Action(..)
  , arguments
  , execParser
  )
where

import           Universum
import           Options.Applicative
import           Types

data Action
  = Sync { filePath :: FilePath
         , syncDestination :: Maybe Text
         , watch    :: Bool }
  deriving (Show)

verboseOpt :: Parser Verbosity
verboseOpt =
  flag Normal Verbose (short 'v' <> help "Log additional information to stdout")

quietOpt :: Parser Verbosity
quietOpt =
  flag' Quiet (long "quiet" <> short 'q' <> help "Dont log any information to stdout")

syncParser :: Parser Action
syncParser =
  Sync
    <$> argument str (metavar "FILEPATH")
    <*> (   Just
        <$> strOption
              (long "destination" <> metavar "DESTINATION" <> help
                "Where to save reminders parsed in FILEPATH"
              )
        <|> flag'
              Nothing
              (  long "default-destination"
              <> help "Use default destination to store todos"
              )
        )
    <*> switch
          (long "watch" <> short 'w' <> help
            "Watch file changes and execute a sync on each change"
          )

configParser :: Parser SyncConfig
configParser = SyncConfig <$> (verboseOpt <|> quietOpt)

data Args = Args
  { action :: Action
  , config :: SyncConfig
  } deriving (Show)

arguments :: ParserInfo Args
arguments = info
  (Args <$> syncParser <*> configParser <**> helper)
  (fullDesc <> progDesc "Sync org file with MacOS Reminders" <> header "Reminders helper")
