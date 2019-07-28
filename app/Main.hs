{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

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
import           Prelude                        ( getChar )
import           Logging

main :: IO ()
main = do
  args@(Args (Sync path toWatch) conf) <- execParser arguments
  canonPath                            <- canonicalizePath path

  result                               <- runO2AM conf $ do

    logDebug $ "Arguments " <> show args

    syncFile canonPath

    threadPerEvent <- reader configThreadPerEvent

    let managerConf = defaultConfig { confThreadPerEvent = threadPerEvent }

    when toWatch $ liftIO . withManagerConf managerConf $ \mgr -> do
      let dir          = takeDirectory path
          shouldUpdate = equalFilePath canonPath . eventPath
          onChange _ =
            runO2AM conf (syncFile canonPath)
              >>= (\case
                    Left  err -> print err
                    Right r   -> pure r
                  )
      stop <- watchDir mgr dir shouldUpdate onChange
      logInfo' (configVerbosity conf) "📝 Listening for changes... Press any key to stop"
      _ <- getChar
      stop

  case result of
    (Left  err) -> print err
    (Right re ) -> return re
 where
  syncFile path = do
    logInfo $ "Processing " <> path
    parseResult <- runParser <$> readFile path

    whenLeft parseResult logError
    whenRight parseResult $ evalAppleScript . sync . reminders
    logDebug "Done"
