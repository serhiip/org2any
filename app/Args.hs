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

data Action
  = Sync { filePath :: FilePath
         , watch    :: Bool }
  deriving (Show)

watchOpt :: Parser Bool
watchOpt = switch (long "watch" <> short 'w' <> help "Watch file changes and execute a sync on each change")

fileArg :: Parser FilePath
fileArg = argument str (metavar "FILEPATH")

syncParser :: Parser Action
syncParser = Sync <$> fileArg <*> watchOpt

data Args = Args
  { action :: Action
  }

arguments :: ParserInfo Args
arguments = info (Args <$> syncParser <**> helper)
                 (fullDesc <> progDesc "Sync org file with MacOS Reminders" <> header "Reminders helper")
