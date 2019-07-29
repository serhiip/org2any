{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Logging
  ( initLogging
  , logDebug
  , logInfo
  , logError
  , logInfo'
  , logError'
  )
where

import           System.Log.FastLogger          ( ToLogStr
                                                , toLogStr
                                                , newTimedFastLogger
                                                , LogType(..)
                                                , TimedFastLogger
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

instance ToLogStr SyncError where
  toLogStr (SysCallError bs) = toLogStr bs

data Severity = Debug | Info | Error deriving (Show, Eq, Ord)

initLogging :: IO (TimedFastLogger, TimedFastLogger, IO ())
initLogging = do
  timeCache                <- newTimeCache simpleTimeFormat'
  (stdoutLogger, cleanUp ) <- newTimedFastLogger timeCache (LogStdout 1)
  (stderrLogger, cleanUp') <- newTimedFastLogger timeCache (LogStderr 1)
  return (stdoutLogger, stderrLogger, cleanUp >> cleanUp')

logMessage' :: ToLogStr a => Verbosity -> (TimedFastLogger, TimedFastLogger) -> Severity -> a -> IO ()
logMessage' verbosity (stdout, stderr) severity message = do
  let logger = if severity == Error then stderr else stdout
  when (canLog verbosity) $ logger
    (\time -> toLogStr time <> toLogStr " " <> toLogStr severity <> toLogStr " " <> toLogStr message <> toLogStr "\n")
 where
  canLog verbosity = case (verbosity, severity) of
    (_     , Error) -> True
    (Normal, Debug) -> False
    (Quiet , _    ) -> False
    _               -> True

logInfo' loggers verbosity = logMessage' verbosity loggers Info

logError' loggers verbosity = logMessage' verbosity loggers Error

logMessageM :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ Bootstrapped) => Severity -> a -> m ()
logMessageM severity message = do
  config <- ask
  liftIO $ logMessage' (configVerbosity . bootstrappedConfig $ config) (bootstrappedLoggers config) severity message

logDebug :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ Bootstrapped) => a -> m ()
logDebug = logMessageM Debug

logInfo :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ Bootstrapped) => a -> m ()
logInfo = logMessageM Info

logError :: (MonadIO m, ToLogStr a, MonadReader r m, r ~ Bootstrapped) => a -> m ()
logError = logMessageM Error

