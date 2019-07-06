module Main where

import           AppleScript                    ( runAppleScript )
import           Args                           ( Action(..)
                                                , Args(..)
                                                , arguments
                                                , execParser
                                                )
import           Command
import           Data.Bifunctor                 ( bimap )
import           Data.Foldable                  ( fold )
import           Data.Text                      ( pack )
import           Data.UUID                      ( toText )
import           Parser                         ( reminders
                                                , runParser
                                                )
import           System.Directory
import           System.FilePath
import           System.FSNotify         hiding ( Action )
import           System.Random                  ( randomIO )
import           Types


main :: IO ()
main = handle =<< execParser arguments
 where

  handle :: Args -> IO ()
  handle (Args (Add body)) = do
    ident <- randomIO
    runAppleScript . create $ Reminder (pack body) (toText ident) (pack "") (Just Todo)
  handle (Args (Sync path False)) = syncFile path
  handle (Args (Sync path True )) = syncFile path >> watchFileChanges path

  syncFile path = runParser . pack <$> readFile path >>= fold . bimap handleError execute

  execute     = runAppleScript . sync . reminders

  handleError = print

  watchFileChanges path = withManagerConf defaultConfig { confThreadPerEvent = False } $ \mgr -> do
    canonPath <- canonicalizePath path
    let dir          = takeDirectory path
        shouldUpdate = equalFilePath canonPath . eventPath
        onChange     = const (syncFile path)
    stop <- watchDir mgr dir shouldUpdate onChange
    putStrLn "Listening for changes..."
    putStrLn "📝 Press <enter> to stop"
    _ <- getChar
    stop
