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
import           Prelude                        ( getChar )

main :: IO ()
main = do
  (Args (Sync path toWatch) conf) <- execParser arguments

  runResult                       <- runO2AM conf $ do

    syncFile path

    threadPerEvent <- reader configThreadPerEvent

    let managerConf = defaultConfig { confThreadPerEvent = threadPerEvent }

    when toWatch
      .  liftIO
      $  withManagerConf managerConf (watchFile path conf)
      >> putStrLn "📝 Listening for changes... Press any key to stop"
      >> getChar
      >> print "c ya!"

  whenLeft runResult print
 where

  syncFile path = do
    parseResult <- runParser <$> readFile path

    whenLeft parseResult putStr
    whenRight parseResult $ evalAppleScript . sync . reminders

  watchFile path conf mgr = do
    canonicalPath <- canonicalizePath path

    let dir          = takeDirectory canonicalPath
        shouldUpdate = equalFilePath canonicalPath . eventPath
        onChange _ = do
          syncResult <- runO2AM conf $ syncFile path
          whenLeft syncResult print
    watchDir mgr dir shouldUpdate onChange
