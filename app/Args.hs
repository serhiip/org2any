{-# LANGUAGE NoImplicitPrelude #-}


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
         , watch    :: Bool }
  deriving (Show)

watchOpt :: Parser Bool
watchOpt = switch (long "watch" <> short 'w' <> help "Watch file changes and execute a sync on each change")

verboseOpt :: Parser Verbosity
verboseOpt = flag Normal Verbose (short 'v' <> help "Log additional information to stdout")

crazyOpt :: Parser Bool
crazyOpt = switch (long "crazy" <> short 'c' <> help "Create parallel threads to handle individual file updates (causes errors now)")

fileArg :: Parser FilePath
fileArg = argument str (metavar "FILEPATH")

syncParser :: Parser Action
syncParser = Sync <$> fileArg <*> watchOpt

configParser :: Parser SyncConfig
configParser = SyncConfig <$> verboseOpt <*> crazyOpt

data Args = Args
  { action :: Action
  , config :: SyncConfig
  } deriving (Show)

arguments :: ParserInfo Args
arguments = info (Args <$> syncParser <*> configParser  <**> helper)
                 (fullDesc <> progDesc "Sync org file with MacOS Reminders" <> header "Reminders helper")
