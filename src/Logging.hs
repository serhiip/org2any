{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Logging
  ( initLogging
  , logDebug
  , logInfo
  , logError
  , logInfo'
  , logError'
  , logDebug'
  )
where

import           Data.Text                      ( unpack )
import qualified Data.Text.Lazy                as TL
import           System.Log.FastLogger          ( ToLogStr
                                                , toLogStr
                                                , newTimedFastLogger
                                                , LogType(..)
                                                , TimedFastLogger
                                                )
import           System.Log.FastLogger.Date     ( newTimeCache
                                                , simpleTimeFormat'
                                                )
import           Types
import           Universum

instance ToLogStr Severity where
  toLogStr Debug = toLogStr $ TL.pack "[DEBUG]"
  toLogStr Info = toLogStr $ TL.pack "[INFO] "
  toLogStr Error = toLogStr $ TL.pack "[ERROR]"

instance ToLogStr SyncError where
  toLogStr (SysCallError bs) = toLogStr bs
  toLogStr (NoItemsError path) =
    toLogStr $ "No org items found to import in " <> show path
  toLogStr (InvalidDestinationError destination) =
    toLogStr $ "There was an error getting reminders from "
    <> unpack destination
    <> ". Try specifying different name"

data Severity = Debug | Info | Error deriving (Show, Eq, Ord)

initLogging :: IO (TimedFastLogger, TimedFastLogger, IO ())
initLogging = do
  timeCache                <- newTimeCache simpleTimeFormat'
  (stdoutLogger, cleanUp ) <- newTimedFastLogger timeCache (LogStdout 1)
  (stderrLogger, cleanUp') <- newTimedFastLogger timeCache (LogStderr 1)
  return (stdoutLogger, stderrLogger, cleanUp >> cleanUp')

logMessage'
  :: ToLogStr a
  => Severity
  -> (TimedFastLogger, TimedFastLogger)
  -> Verbosity
  -> a
  -> IO ()
logMessage' severity (stdo, stde) verbosity message = do
  let logger = if severity == Error then stde else stdo
  when canLog $ logger
    (\time ->
      toLogStr time
        <> toLogStr " "
        <> toLogStr severity
        <> toLogStr " "
        <> toLogStr message
        <> toLogStr "\n"
    )
 where
  canLog = case (verbosity, severity) of
    (_     , Error) -> True
    (Normal, Debug) -> False
    (Quiet , _    ) -> False
    _               -> True

logInfo' :: ToLogStr a => (TimedFastLogger, TimedFastLogger) -> Verbosity -> a -> IO ()
logInfo' = logMessage' Info

logError' :: ToLogStr a => (TimedFastLogger, TimedFastLogger) -> Verbosity -> a -> IO ()
logError' = logMessage' Error

logDebug' :: ToLogStr a => (TimedFastLogger, TimedFastLogger) -> Verbosity -> a -> IO ()
logDebug' = logMessage' Debug

logMessageM
  :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ Bootstrapped) => Severity -> a -> m ()
logMessageM severity message = do
  config <- ask
  liftIO $ logMessage' severity
                       (bootstrappedLoggers config)
                       (configVerbosity . bootstrappedConfig $ config)
                       message

logDebug :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ Bootstrapped) => a -> m ()
logDebug = logMessageM Debug

logInfo :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ Bootstrapped) => a -> m ()
logInfo = logMessageM Info

logError :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ Bootstrapped) => a -> m ()
logError = logMessageM Error
