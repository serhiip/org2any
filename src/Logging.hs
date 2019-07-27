{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Logging
  ( logDebug
  , logInfo
  , logError
  )
where

import           System.Log.FastLogger          ( ToLogStr
                                                , toLogStr
                                                , newTimedFastLogger
                                                , LogType(..)
                                                )
import           System.Log.FastLogger.Date     ( newTimeCache
                                                , simpleTimeFormat'
                                                )
import qualified Data.Text.Lazy                as TL
import           Types

import           Universum

instance ToLogStr Severity where
  toLogStr Debug = toLogStr $ TL.pack "[DEBUG]"
  toLogStr Info = toLogStr $ TL.pack "[INFO] "
  toLogStr Error = toLogStr $ TL.pack "[ERROR]"

data Severity = Debug | Info | Error deriving (Show, Eq, Ord)

logMessage :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ SyncConfig) => Severity -> a -> m ()
logMessage severity message = do
  verbosity         <- reader configVerbosity
  timeCache         <- liftIO $ newTimeCache simpleTimeFormat'
  (logger, cleanUp) <- liftIO $ newTimedFastLogger timeCache (LogStdout 100)
  liftIO $ when (canLog verbosity) $ logger
    (\time -> toLogStr time <> toLogStr " " <> toLogStr severity <> toLogStr " " <> toLogStr message <> toLogStr "\n")
  liftIO cleanUp
 where
  canLog verbosity = case (verbosity, severity) of
    (Normal , Info ) -> True
    (Verbose, Info ) -> True
    (Normal , Debug) -> False
    (Verbose, Debug) -> True
    (_      , Error) -> True

logDebug :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ SyncConfig) => a -> m ()
logDebug = logMessage Debug

logInfo :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ SyncConfig) => a -> m ()
logInfo = logMessage Info

logError :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ SyncConfig) => a -> m ()
logError = logMessage Error
