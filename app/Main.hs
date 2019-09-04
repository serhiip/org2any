{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           AppleScript                    ( evalAppleScript )
import           Args                           ( Action(..)
                                                , Args(..)
                                                , arguments
                                                , execParser
                                                )
import           Command
import           Parser                         ( reminders
                                                , runParser
                                                )
import           System.Directory
import           System.FilePath
import           System.FSNotify         hiding ( Action )
import           Types

import           Universum
import           Universum.Lifted.File          ( readFile )
import           Control.Monad.Except           ( throwError )
import           Logging

main :: IO ()
main = do
  args@(Args (Sync path toWatch) conf) <- execParser arguments
  canonPath                            <- canonicalizePath path
  (stdoutLogger, stderrLogger, loggingCleanUp) <- initLogging

  let loggers   = (stdoutLogger, stderrLogger)
      verbosity = configVerbosity conf
      bootstrap = Bootstrapped conf loggers

  result <- runO2AM bootstrap $ do

    logDebug $ "Arguments " <> show args

    syncFile canonPath

    threadPerEvent <- reader (configThreadPerEvent . bootstrappedConfig)

    let managerConf = defaultConfig { confThreadPerEvent = threadPerEvent }

    when toWatch $ liftIO . withManagerConf managerConf $ \mgr -> do
      let dir          = takeDirectory path
          shouldUpdate = equalFilePath canonPath . eventPath
          onChange _ = do
            result <- runO2AM bootstrap (syncFile canonPath)
            whenLeft result $ logError' loggers verbosity
            whenRight result pure
      stop <- watchDir mgr dir shouldUpdate onChange
      logInfo' loggers (configVerbosity conf) "📝 Listening for changes... Press any key to stop"
      line <- getLine
      logDebug' loggers verbosity line
      stop

  whenLeft result $ logError' loggers verbosity
  loggingCleanUp
 where
  syncFile :: FilePath -> O2AM ()
  syncFile path = do
    logInfo $ "Processing " <> path
    parsed <- runParser <$> readFile path

    whenLeft parsed logError
    whenRight
      parsed
      (\orgTree ->
        let items = reminders orgTree
        in  if null items then throwError (NoItemsError path) else evalAppleScript . sync $ items
      )
    logDebug "Done"
