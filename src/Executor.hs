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
import           Control.Monad.Except           ( throwError, liftEither )
import           AppleScript                    ( evalAppleScript )
import           Universum.Exception            ( try )
import           Data.Bifunctor                 ( bimap )



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
      fileContents <- (try . readFile) filePath :: O2AM (Either SomeException Text)
      contents <- liftEither $ bimap (SysCallError . show) id fileContents
      orgTree <- liftEither $ runParser contents

      let items = reminders orgTree
        in  if null items
            then throwError (NoItemsError filePath)
            else evalAppleScript . sync $ items

      logDebug "Done"
