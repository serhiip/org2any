{-|
Module      : Data.OrgMode.Sync.Logging
Description : Logging utilities
License     : GPL-3
Maintainer  : Serhii <serhii@proximala.bz>
Stability   : experimental

Log meassages of various log levels either via main transfromer stack
 or via just IO. Uses
 <https://github.com/kazu-yamamoto/logger kazu-yamamoto/logger>
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.OrgMode.Sync.Logging
  ( initLogging
  , logInfo'
  , logError'
  , logDebug'
  , MonadLogger(..)
  )
where

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
import           Data.OrgMode.Sync.Types
import           Universum

instance ToLogStr Severity where
  toLogStr Debug = toLogStr $ TL.pack "[DEBUG]"
  toLogStr Info  = toLogStr $ TL.pack "[INFO] "
  toLogStr Error = toLogStr $ TL.pack "[ERROR]"

-- | Logging severity and destination (STDOUT / STDERR)
data Severity
  = Debug
  | Info
  | Error
  deriving (Show, Eq, Ord)

-- | Helper function to initialize stderr and stdout loggers
initLogging :: IO (TimedFastLogger, TimedFastLogger, IO ())
initLogging = do
  timeCache                <- newTimeCache simpleTimeFormat'
  (stdoutLogger, stdoutCleanUp ) <- newTimedFastLogger timeCache (LogStdout 1)
  (stderrLogger, stderrCleanUp') <- newTimedFastLogger timeCache (LogStderr 1)
  return (stdoutLogger, stderrLogger, stdoutCleanUp >> stderrCleanUp')

-- | Generic function to log messages. Logs messages of @Error@
-- severity to stderr. Will not emit any messages to stdout when
-- `Data.OrgMode.Sync.Types.Verbosity` is
-- `Data.OrgMode.Sync.Types.Quiet`
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

-- | Print informatic message to STDOUT. Will not emit anything if
-- verbosity is `Data.OrgMode.Sync.Types.Quiet`
logInfo' :: ToLogStr a => (TimedFastLogger, TimedFastLogger) -> Verbosity -> a -> IO ()
logInfo' = logMessage' Info

-- | Print error to STDERR. Ignores verbosity parameter
logError' :: ToLogStr a => (TimedFastLogger, TimedFastLogger) -> Verbosity -> a -> IO ()
logError' = logMessage' Error

-- | Print debug message to STDOUT when verbosity is
-- `Data.OrgMode.Sync.Types.Verbose`
logDebug' :: ToLogStr a => (TimedFastLogger, TimedFastLogger) -> Verbosity -> a -> IO ()
logDebug' = logMessage' Debug

logMessageM :: (MonadReader Bootstrapped m, MonadIO m, ToLogStr a) => Severity -> a -> m ()
logMessageM severity message = do
  loggers <- bootstrappedLoggers <$> ask
  verbosity <- configVerbosity . bootstrappedConfig <$> ask
  liftIO $ logMessage' severity loggers verbosity message

class MonadLogger m where

  logDebug :: ToLogStr a => a -> m ()

  logInfo :: ToLogStr a => a -> m ()

  logError :: ToLogStr a => a -> m ()

instance (MonadIO m, MonadReader Bootstrapped m) => MonadLogger (ResultT m) where

  logDebug = logMessageM Debug

  logInfo = logMessageM Info

  logError = logMessageM Error
