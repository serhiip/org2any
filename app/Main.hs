module Main where

import           AppleScript      (runAppleScript)
import           Args             (Action (..), Args (..), arguments,
                                   execParser)
import           Command
import           Data.Bifunctor   (bimap)
import           Data.Foldable    (fold)
import           Data.Text        (pack)
import           Parser           (reminders, runParser)
import           System.Directory
import           System.FilePath
import           System.FSNotify  hiding (Action)
import           Types
import           Data.UUID        (toText)
import           System.Random    (randomIO)

main :: IO ()
main = handle =<< execParser arguments
  where
    handle :: Args -> IO ()
    handle (Args (Add body)) = do
      ident <- randomIO
      runAppleScript . create $ Reminder (pack body) (toText ident) (pack "") (Just Todo)
    handle (Args (Sync path toWatch)) =
      if toWatch
      then withManager $ \mgr -> execute >> do
        canonPath <- canonicalizePath path
        stop <-
          watchDir
          mgr
          (takeDirectory path)
          (equalFilePath canonPath . eventPath)
          (const execute)
        putStrLn "Listening for changes..."
        putStrLn "📝 Press <enter> to stop"
        _ <- getChar
        stop
      else execute
      where
        execute =
          runParser . pack <$> readFile path >>=
          fold . bimap print (runAppleScript . sync . reminders)
