{-# LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE TypeApplications #-}

module Main where

import           Args                           ( Action(..)
                                                , Args(..)
                                                , arguments
                                                , execParser
                                                )
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
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
import           System.Posix.Signals           ( sigTERM
                                                , sigINT
                                                , installHandler
                                                , Handler(..)
                                                )
import           Control.Exception              ( AsyncException(..)
                                                , handle
                                                )

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
      term      = CatchOnce $ send SystemTerminatedEvent

  _ <- installHandler sigTERM term Nothing
  _ <- installHandler sigINT term Nothing

  debug $ "Arguments " <> show args

  send (SyncEvent path dst)

  _ <- forkIO . forever $ do
    result <- runResult bootstrap execute
    whenLeft result error'

  unless toWatch $ send EndEvent

  watcher <- forkIO . when toWatch $ withManagerConf
    defaultConfig
    (\manager -> do
      watchManagerCleanUp <- watchDir manager
                                      (takeDirectory path)
                                      (equalFilePath canonPath . eventPath)
                                      (const $ send (SyncEvent path dst))
      let stop = watchManagerCleanUp >> debug "File watcher was stopped"
      handle @AsyncException (const stop) $ do
        info "📝 Listening for changes... Press any key to stop"
        line <- getLine
        stop
        send $ UserTerminatedEvent line
    )

  readChan outputChan
  killThread watcher
  debug "Logging handlers cleaned up"
  loggingCleanUp
