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
  (Args (Sync path toWatch)) <- execParser arguments

  runO2AM (SyncConfig True) $ do

    syncFile path

    when toWatch $ liftIO $ withManagerConf defaultConfig { confThreadPerEvent = False } $ \mgr -> do
      canonPath <- canonicalizePath path

      let dir          = takeDirectory path
          shouldUpdate = equalFilePath canonPath . eventPath
          onChange     = const (syncFile path)
      stop <- watchDir mgr dir shouldUpdate (runO2AM (SyncConfig True) . onChange)
      putStrLn "Listening for changes..."
      putStrLn "📝 Press <enter> to stop"
      _ <- getChar
      stop
 where
  syncFile path = do
    parsed <- runParser <$> readFile path

    whenLeft parsed putStr
    whenRight parsed $ liftIO . runAppleScript . sync . reminders
