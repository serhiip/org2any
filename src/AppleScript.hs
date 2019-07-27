{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module AppleScript
  ( evalAppleScript
  )
where

import           Universum

import           Command                        ( Command
                                                , CommandF(..)
                                                )
import           Control.Monad.Free             ( Free(..) )

import           Data.Text                      ( Text )
import           System.Process.Typed           ( proc
                                                , readProcess
                                                )
import           Types
import           AppleScript.Internal
import           System.Exit                    ( ExitCode(..) )
import           Control.Monad.Except           ( throwError )

execute :: String -> O2AM Text
execute script = do
  (exitCode, out, err) <- liftIO . readProcess $ proc "/usr/bin/osascript" args
  if exitCode == ExitSuccess
    then pure $ decodeUtf8 @Text @LByteString out
    else throwError $ SysCallError err
  where args = ["-l", "JavaScript", "-e", script]

evalAppleScript :: Command x -> O2AM x
evalAppleScript (Pure r         ) = return r
evalAppleScript (Free (GetAll f)) = do
  reminders <- decodeRemindersList <$> execute listAllScript
  evalAppleScript . f $ reminders
evalAppleScript (Free CreateMany {..}) = do
  _ <- execute . createManyScript $ rs
  evalAppleScript x
evalAppleScript (Free DeleteMany {..}) = do
  _ <- execute . deleteManyScript $ rs
  evalAppleScript x
evalAppleScript (Free UpdateAll {..}) = do
  _ <- execute . updateManyScript $ rs
  evalAppleScript x
