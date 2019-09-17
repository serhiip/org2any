{-# LANGUAGE OverloadedStrings #-}

module AppleScript
  ( evalAppleScript
  )
where

import           AppleScript.Internal
import           Command                        ( Command
                                                , CommandF(..)
                                                )
import           Control.Monad.Except           ( throwError
                                                , liftEither
                                                )
import           Control.Monad.Free             ( Free(..) )
import           Data.Aeson                     ( eitherDecode )
import           Data.Bifunctor                 ( first )
import qualified Data.Set                      as S
import           System.Exit                    ( ExitCode(..) )
import           System.Process.Typed           ( proc
                                                , readProcess
                                                )
import           Types
import           Universum

execute :: LByteString -> Result LByteString
execute script = do
  (exitCode, out, err) <- liftIO . readProcess $ proc
    "/usr/bin/osascript"
    ["-l", "JavaScript", "-e", decodeUtf8 script]
  unless (exitCode == ExitSuccess) $ throwError (SysCallError $ decodeUtf8 err)
  return out

evalAppleScript :: Command x -> Result x
evalAppleScript (Pure r                ) = return r
evalAppleScript (Free (GetAll bucket f)) = do
  decoded   <- decodeRemindersList <$> execute (listAllScript $ bucketId bucket)
  reminders <- liftEither $ first SysCallError decoded
  evalAppleScript . f $ reminders
evalAppleScript (Free (CreateMany bucket rs rest)) = do
  _ <- execute . createManyScript (bucketId bucket) $ convert <$> remindersToList rs
  evalAppleScript rest
evalAppleScript (Free (DeleteMany bucket rs rest)) = do
  _ <- execute . deleteManyScript (bucketId bucket) $ convert <$> remindersToList rs
  evalAppleScript rest
evalAppleScript (Free (UpdateAll bucket rs rest)) = do
  _ <- execute . updateManyScript (bucketId bucket) $ convert <$> remindersToList rs
  evalAppleScript rest
evalAppleScript (Free (ListBuckets f)) = do
  result  <- eitherDecode <$> execute listListsScript
  buckets <- liftEither $ first (SysCallError . fromString) result
  evalAppleScript . f . S.map convertBucket $ buckets
