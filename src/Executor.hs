{-# LANGUAGE NoImplicitPrelude #-}

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
import           Logging
import           Parser                         ( reminders
                                                , runParser
                                                )
import           Types
import           Universum


execute :: O2AM ()
execute = do
  input  <- reader bootstrappedInput
  output <- reader bootstrappedOutput
  event  <- liftIO $ readChan input
  let notifyEnd = liftIO $ writeChan output ()
  case event of
    EndEvent                      -> notifyEnd
    UserTerminatedEvent lastWords -> logDebug lastWords *> notifyEnd
    SystemTerminatedEvent         -> logError "org2any was terminated" *> notifyEnd
    SyncEvent filePath            -> do
      logInfo $ "Processing " <> filePath
      fileContents <- (try . readFile) filePath :: O2AM (Either IOException Text)
      contents     <- liftEither $ first (SysCallError . show) fileContents
      orgTree      <- liftEither $ runParser contents

      let items = reminders orgTree

      if null items
        then throwError (NoItemsError filePath)
        else (evalAppleScript . sync) items

      logDebug "Done"
