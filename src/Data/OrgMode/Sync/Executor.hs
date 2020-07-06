{-|
Module      : Data.OrgMode.Sync.Executor
Description : Handler of events of interaction
License     : GPL-3
Maintainer  : Serhii <serhii@proximala.bz>
Stability   : experimental

Handles the event coming to the program: either user events
 (file update, program stop) or system events (termination or exception)
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.OrgMode.Sync.Executor
  ( execute
  , handle
  )
where

import           Data.OrgMode.Sync.Command
import           Control.Concurrent.Chan        ( readChan
                                                , writeChan
                                                )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                , liftEither
                                                )
import           Data.Text                      ( pack )
import           Data.OrgMode.Sync.Logging
import           Data.OrgMode.Sync.Parser       ( reminders
                                                , runParser
                                                )
import           Data.OrgMode.Sync.Types
import           Universum

-- | Event handler listens for input channel supplied in
-- `Data.OrgMode.Sync.Types.Bootstrapped` and executes the handlers
-- based on the type of event in parallel to the main thread of
-- execution. Handlers are executed -- synchronously - new incomming
-- request will be queued up in an input channel if this thread is
-- busy executing previous request

execute
  :: ( MonadIO m
     , MonadFileReader m
     , MonadReader Bootstrapped m
     , MonadLogger m
     , MonadError SyncError m
     , MonadCommandEvaluator m
     )
  => m ()
execute = do
  input  <- reader bootstrappedInput
  output <- reader bootstrappedOutput
  event  <- liftIO $ readChan input
  let notifyEnd = liftIO $ writeChan output ()
  case event of
    EndEvent                      -> notifyEnd
    UserTerminatedEvent lastWords -> logDebug lastWords *> notifyEnd
    SystemTerminatedEvent         -> logError (pack "org2any was terminated") *> notifyEnd
    SyncEvent filePath dst        -> handle $ SyncAction filePath dst

handle
  :: ( MonadFileReader m
     , MonadReader Bootstrapped m
     , MonadLogger m
     , MonadError SyncError m
     , MonadCommandEvaluator m
     )
  => ActionType
  -> m ()
handle (SyncAction filePath destination) = do
  logInfo $ "Processing " <> filePath
  readResult <- readFileM filePath
  contents   <- liftEither $ first (FileReadError filePath . show) readResult
  orgTree    <- liftEither $ runParser contents

  let items = reminders orgTree
      name  = destination ?: "Reminders"

  when (null items) (throwError $ NoItemsError filePath)

  result <- evaluate OSXReminders . sync name $ items
  liftEither result

  logDebug $ "Done synchronizing " <> show filePath <> " to " <> name
