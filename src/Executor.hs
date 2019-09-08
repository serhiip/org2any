{-# LANGUAGE NoImplicitPrelude #-}

module Executor
  ( execute
  )
where

import           Types
import           Universum
import           Control.Concurrent.Chan        ( readChan
                                                , writeChan
                                                )
import           Command
import           Parser                         ( reminders
                                                , runParser
                                                )
import           Control.Monad.Except           ( throwError )
import           AppleScript                    ( evalAppleScript )


import           Logging

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

      parsed <- runParser <$> readFile filePath

      whenLeft parsed logError
      whenRight
        parsed
        (\orgTree ->
          let items = reminders orgTree
          in  if null items
              then throwError (NoItemsError filePath)
              else evalAppleScript . sync $ items
        )

      logDebug "Done"
