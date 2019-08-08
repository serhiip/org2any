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

import           Data.Text                      ( Text
                                                , pack
                                                )
import           System.Process.Typed           ( proc
                                                , readProcess
                                                )
import           Types
import           AppleScript.Internal
import           System.Exit                    ( ExitCode(..) )
import           Control.Monad.Except           ( throwError )

execute :: String -> O2AM LByteString
execute script = do
  (exitCode, out, err) <- liftIO . readProcess $ proc "/usr/bin/osascript" args
  if exitCode == ExitSuccess then pure out else throwError $ SysCallError err
  where args = ["-l", "JavaScript", "-e", script]

evalAppleScript :: Command x -> O2AM x
evalAppleScript (Pure r         ) = return r
evalAppleScript (Free (GetAll f)) = do
  reminders <- decodeRemindersList <$> execute listAllScript
  case reminders of
    Left  err  -> throwError . SysCallError $ encodeUtf8 @Text @LByteString (pack err)
    Right rems -> evalAppleScript . f $ rems
evalAppleScript (Free CreateMany {..}) = do
  _ <- execute . createManyScript $ rs
  evalAppleScript x
evalAppleScript (Free DeleteMany {..}) = do
  _ <- execute . deleteManyScript $ rs
  evalAppleScript x
evalAppleScript (Free UpdateAll {..}) = do
  _ <- execute . updateManyScript $ rs
  evalAppleScript x
