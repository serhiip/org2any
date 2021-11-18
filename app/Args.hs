module Args
  ( Args(..)
  , Action(..)
  , arguments
  , execParser
  ) where

import           Data.OrgMode.Sync.Types        ( SyncConfig(SyncConfig)
                                                , Verbosity(..)
                                                )
import           Options.Applicative
import           Universum

data Action = Sync
  { filePath        :: FilePath
  , syncDestination :: Maybe Text
  , watch           :: Bool
  } | Version
  deriving Show

verboseOpt :: Parser Verbosity
verboseOpt =
  flag Normal Verbose (short 'v' <> help "Log additional information to stdout")

quietOpt :: Parser Verbosity
quietOpt = flag'
  Quiet
  (long "quiet" <> short 'q' <> help "Dont log any information to stdout")

version :: Parser Action
version =
  flag' Version (long "version" <> short 'v' <> help "Print version and exit")

actionParser :: Parser Action
actionParser =
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
    <|> version

configParser :: Parser SyncConfig
configParser = SyncConfig <$> (verboseOpt <|> quietOpt)

data Args = Args
  { action :: Action
  , config :: SyncConfig
  }
  deriving Show

arguments :: ParserInfo Args
arguments = info
  (Args <$> actionParser <*> configParser <**> helper)
  (fullDesc <> progDesc "Sync org file with MacOS Reminders" <> header
    "Reminders helper"
  )
