{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
import           Control.Monad.IO.Class         ( MonadIO )


import           Data.Text                      ( Text )
import           System.Process.Typed           ( proc
                                                , readProcessStdout_
                                                )
import           Types
import           AppleScript.Internal

execute :: MonadIO m => String -> m Text
execute script = do
  outBS <- readProcessStdout_ $ proc "/usr/bin/osascript" args
  return $ decodeUtf8 @Text @LByteString outBS
  where args = ["-l", "JavaScript", "-e", script]

evalAppleScript :: Command x -> O2AM x
evalAppleScript (Pure r         ) = return r
evalAppleScript (Free (GetAll f)) = do
  reminders <- liftIO $ decodeRemindersList <$> execute listAllScript
  evalAppleScript . f $ reminders
evalAppleScript (Free CreateMany {..}) = do
  _ <- liftIO . execute . createManyScript $ rs
  evalAppleScript x
evalAppleScript (Free DeleteMany {..}) = do
  _ <- liftIO . execute . deleteManyScript $ rs
  evalAppleScript x
evalAppleScript (Free UpdateAll {..}) = do
  _ <- liftIO . execute . updateManyScript $ rs
  evalAppleScript x
