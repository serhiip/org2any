{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Args                           ( Action(..)
                                                , Args(..)
                                                , arguments
                                                , execParser
                                                )
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Chan        ( newChan
                                                , writeChan
                                                , readChan
                                                )
import           Control.Monad                  ( forever )
import           Executor                       ( execute )
import           Logging
import           System.Directory
import           System.FSNotify         hiding ( Action )
import           System.FilePath
import           Types
import           Universum

main :: IO ()
main = do
  args@(Args (Sync path dst toWatch) conf) <- execParser arguments
  canonPath                    <- canonicalizePath path
  (stdo, stde, loggingCleanUp) <- initLogging
  inputChan                    <- newChan
  outputChan                   <- newChan

  let loggers   = (stdo, stde)
      verbosity = configVerbosity conf
      bootstrap = Bootstrapped conf loggers inputChan outputChan
      send      = writeChan inputChan
      debug     = logDebug' loggers verbosity
      info      = logInfo' loggers verbosity
      error'    = logError' loggers verbosity

  debug $ "Arguments " <> show args

  send (SyncEvent path dst)

  _ <- forkIO . forever $ do
    result <- runResult bootstrap execute
    whenLeft result error'

  unless toWatch $ send EndEvent

  when toWatch $ withManagerConf defaultConfig $ \manager -> do
    watchManagerCleanUp <- watchDir manager
                                    (takeDirectory path)
                                    (equalFilePath canonPath . eventPath)
                                    (const $ send (SyncEvent path dst))
    info "📝 Listening for changes... Press any key to stop"
    line <- getLine
    debug "Stopping file watcher"
    watchManagerCleanUp
    send $ UserTerminatedEvent line

  readChan outputChan
  debug "Cleaning up logging handlers"
  loggingCleanUp
