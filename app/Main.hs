{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           AppleScript                    ( runAppleScript )
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

  runO2AM conf $ do

    syncFile path

    threadPerEvent <- reader configThreadPerEvent

    let managerConf = defaultConfig { confThreadPerEvent = threadPerEvent }

    when toWatch $ liftIO . withManagerConf managerConf $ \mgr -> do
      canonPath <- canonicalizePath path

      let dir          = takeDirectory path
          shouldUpdate = equalFilePath canonPath . eventPath
          onChange     = runO2AM conf . const (syncFile path)
      stop <- watchDir mgr dir shouldUpdate onChange
      putStrLn "📝 Listening for changes... Press any key to stop"
      _ <- getChar
      stop
 where
  syncFile path = do
    parsed <- runParser <$> readFile path

    whenLeft parsed putStr
    whenRight parsed $ liftIO . runAppleScript . sync . reminders
