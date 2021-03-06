{-|
Module      : Data.OrgMode.Sync.AppleScript
Description : AppleScript interpreter of `Command.Command`s
License     : GPL-3
Maintainer  : Serhii <serhii@proximala.bz>
Stability   : experimental

Executes the sequence of `Data.OrgMode.Sync.Command.Command`s to
 syncronize a list of reminders to Reminders application on Mac OS.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.OrgMode.Sync.AppleScript
  ( evalAppleScript
  )
where

import           Data.OrgMode.Sync.AppleScript.Internal
import           Control.Monad.Except           ( throwError
                                                , liftEither
                                                , MonadError(..)
                                                )
import           Control.Monad.Free             ( Free(..) )
import           Data.Aeson                     ( eitherDecode )
import           System.Exit                    ( ExitCode(..) )
import           System.Process.Typed           ( proc
                                                , readProcess
                                                )
import           Data.OrgMode.Sync.Types
import           Universum
import           Data.OrgMode.Sync.Logging

execute
  :: (MonadIO m, MonadLogger m, MonadError SyncError m) => LByteString -> m LByteString
execute script = do
  (exitCode, out, err) <- liftIO . readProcess $ proc
    "/usr/bin/osascript"
    ["-l", "JavaScript", "-e", decodeUtf8 script]
  logDebug $ "Executing AppleScript \n" <> script
  unless (exitCode == ExitSuccess) $ throwError (SysCallError $ decodeUtf8 err)
  return out

-- | Execute commands by producing calls (side effects) to Reminders
-- OS X application via
-- <https://www.unix.com/man-page/osx/1/osascript/ osascript> system
-- utility
evalAppleScript :: (MonadIO m, MonadLogger m, MonadError SyncError m) => Command x -> m x
evalAppleScript (Pure r                ) = return r
evalAppleScript (Free (GetAll bucket f)) = do
  raw          <- execute (listAllScript $ bucketId bucket)
  osxReminders <- liftEither
    $ first (DecodeError $ decodeUtf8 raw) (decodeOSXReminders raw)
  evalAppleScript . f . mapMaybe from $ osxReminders

evalAppleScript (Free (CreateMany bucket rs rest)) = do
  _ <- execute . createManyScript (bucketId bucket) $ to <$> rs
  evalAppleScript rest
evalAppleScript (Free (DeleteMany bucket rs rest)) = do
  _ <- execute . deleteManyScript (bucketId bucket) $ to <$> rs
  evalAppleScript rest
evalAppleScript (Free (UpdateAll bucket rs rest)) = do
  _ <- execute . updateManyScript (bucketId bucket) $ to <$> rs
  evalAppleScript rest
evalAppleScript (Free (ListBuckets f)) = do
  result  <- eitherDecode <$> execute listListsScript
  buckets <- liftEither $ first (SysCallError . fromString) result
  evalAppleScript . f . fmap convertBucket $ buckets
