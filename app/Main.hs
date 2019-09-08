{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Args                           ( Action(..)
                                                , Args(..)
                                                , arguments
                                                , execParser
                                                )
import           System.Directory
import           System.FilePath
import           System.FSNotify         hiding ( Action )
import           Types

import           Universum
import           Logging
import           Control.Concurrent.Chan        ( newChan
                                                , writeChan
                                                , readChan
                                                )
import           Control.Concurrent             ( forkIO )
import           Executor                       ( execute )
import           Control.Monad                  ( forever )

main :: IO ()
main = do
  args@(Args (Sync path toWatch) conf) <- execParser arguments
  canonPath                            <- canonicalizePath path
  (stdo, stde, loggingCleanUp)         <- initLogging
  inputChan                            <- newChan
  outputChan                           <- newChan

  let loggers   = (stdo, stde)
      verbosity = configVerbosity conf
      bootstrap = Bootstrapped conf loggers inputChan outputChan
      send      = writeChan inputChan
      debug     = logDebug' loggers verbosity
      info      = logInfo' loggers verbosity
      errorr    = logError' loggers verbosity

  debug $ "Arguments " <> show args

  _ <- send (SyncEvent path)

  _ <- forkIO . forever $ do
    result <- runO2AM bootstrap execute
    whenLeft result errorr

  unless toWatch $ send EndEvent

  when toWatch $ withManagerConf defaultConfig $ \manager -> do
    watchManagerCleanUp <- watchDir manager
                                    (takeDirectory path)
                                    (equalFilePath canonPath . eventPath)
                                    (const $ send (SyncEvent path))
    info "📝 Listening for changes... Press any key to stop"
    line <- getLine
    debug "Stopping file watcher"
    watchManagerCleanUp
    send $ UserTerminatedEvent line

  readChan outputChan
  debug "Cleaning up logging handlers"
  loggingCleanUp
