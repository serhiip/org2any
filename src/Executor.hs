{-# LANGUAGE OverloadedStrings #-}

module Executor
  ( execute
  )
where

import           AppleScript                    ( evalAppleScript )
import           Command
import           Control.Concurrent.Chan        ( readChan
                                                , writeChan
                                                )
import           Control.Exception              ( IOException )
import           Control.Monad.Except           ( throwError
                                                , liftEither
                                                )
import           Data.Text                      ( pack )
import           Logging
import           Parser                         ( reminders
                                                , runParser
                                                )
import           Types
import           Universum

execute :: UnitResult
execute = do
  input  <- reader bootstrappedInput
  output <- reader bootstrappedOutput
  event  <- liftIO $ readChan input
  let notifyEnd = liftIO $ writeChan output ()
  case event of
    EndEvent                      -> notifyEnd
    UserTerminatedEvent lastWords -> logDebug lastWords *> notifyEnd
    SystemTerminatedEvent         -> logError
                                     (pack "org2any was terminated")
                                     *> notifyEnd
    SyncEvent filePath dst        -> do
      logInfo $ "Processing " <> filePath
      readResult <- (try . readFile) filePath :: Result (Either IOException Text)
      contents   <- liftEither $ first (SysCallError . show) readResult
      orgTree    <- liftEither $ runParser contents

      let items = reminders orgTree
          name  = dst ?: "Reminders"

      when (null items) $ throwError (NoItemsError filePath)

      result <- evalAppleScript . sync name $ items
      liftEither result

      logDebug $ "Done synchronizing " <> show filePath <> " to " <> name
