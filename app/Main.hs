{-# LANGUAGE TypeApplications #-}

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
                                                , readChan
                                                , writeChan
                                                )
import           Control.Exception              ( AsyncException(..)
                                                , handle
                                                )
import           Data.OrgMode.Sync.Executor     ( execute )
import           Data.OrgMode.Sync.Logging      ( initLogging
                                                , logDebug'
                                                , logError'
                                                , logInfo'
                                                )
import           Data.OrgMode.Sync.ResultT      ( runResult )
import           Data.OrgMode.Sync.Types        ( Bootstrapped(Bootstrapped)
                                                , Event
                                                  ( EndEvent
                                                  , SyncEvent
                                                  , SystemTerminatedEvent
                                                  , UserTerminatedEvent
                                                  )
                                                , SyncConfig(configVerbosity)
                                                )
import           System.Directory               ( canonicalizePath )
import           System.FSNotify                ( defaultConfig
                                                , eventPath
                                                , watchDir
                                                , withManagerConf
                                                )
import           System.FilePath                ( equalFilePath
                                                , takeDirectory
                                                )
import           System.Posix.Signals           ( Handler(..)
                                                , installHandler
                                                , sigINT
                                                , sigTERM
                                                )
import           Universum

main :: IO ()
main = do
  args <- execParser arguments
  case args of
    (Args (Sync path dst toWatch) conf) -> do
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
      send EndEvent

      _ <- forkIO . forever $ do
        result <- liftIO $ runResult bootstrap execute
        whenLeft result error'

      unless toWatch $ send EndEvent

      watcher <- forkIO . when toWatch $ withManagerConf
        defaultConfig
        (\manager -> do
          watchManagerCleanUp <- watchDir
            manager
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

    (Args Version _) -> putStrLn "ORG2ANY_VERSION"
