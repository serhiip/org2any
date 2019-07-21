{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module AppleScript
  ( runAppleScript
  )
where

import           Universum

import           Command                        ( Command
                                                , CommandF(..)
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Free
import           Control.Monad.IO.Class         ( MonadIO )


import           Data.Text                      ( Text )
import           System.Process.Typed           ( proc
                                                , readProcessStdout_
                                                )
import           Types                          ( Reminders )
import           AppleScript.Internal


execute :: MonadIO m => String -> m Text
execute script = do
  outBS <- readProcessStdout_ $ proc "/usr/bin/osascript" args
  return $ decodeUtf8 @Text @LByteString outBS
  where args = ["-l", "JavaScript", "-e", script]

createMany :: MonadIO m => Reminders -> m ()
createMany = void . execute . createManyScript

list :: MonadIO m => m Reminders
list = decodeRemindersList <$> execute listAllScript

deleteMany :: MonadIO m => Reminders -> m ()
deleteMany = void . execute . deleteManyScript

updateMany :: MonadIO m => Reminders -> m ()
updateMany = void . execute . updateManyScript

runAppleScript :: Command x -> IO x
runAppleScript (Pure r                ) = return r
runAppleScript (Free (GetAll f       )) = list >>= runAppleScript . f
runAppleScript (Free (CreateMany rs x)) = createMany rs >> runAppleScript x
runAppleScript (Free (DeleteMany rs x)) = deleteMany rs >> runAppleScript x
runAppleScript (Free (UpdateAll  rs x)) = updateMany rs >> runAppleScript x
