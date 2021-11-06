{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.OrgMode.Sync.ResultT
  ( runResultT
  , ResultT(..)
  , runResult
  ) where

import           Control.Monad.Except           ( MonadError(..) )
import           Data.OrgMode.Sync.AppleScript  ( evalAppleScript )
import           Data.OrgMode.Sync.Logging      ( MonadLogger(..)
                                                , logDebugM
                                                , logErrorM
                                                , logInfoM
                                                )
import           Data.OrgMode.Sync.Types        ( Bootstrapped
                                                , MonadCommandEvaluator(..)
                                                , MonadFileReader(..)
                                                , StoreType(InMemory, OSXReminders)
                                                , SyncError
                                                )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( hGetContents )
import           Universum

-- | Main transformers stack. Allows to do IO, monadic computation,
-- have common context (via Reader) and work with pure exceptions
newtype ResultT m a = O2AMT
  { getResultT :: ExceptT SyncError (ReaderT Bootstrapped m) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader Bootstrapped
  , MonadError SyncError
  , MonadThrow
  , MonadCatch
  , MonadMask
  , MonadIO
  )

-- | Execute trasformer stack
runResult :: Bootstrapped -> ResultT IO a -> IO (Either SyncError a)
runResult = runResultT

instance MonadIO m => MonadFileReader (ResultT m) where
  readFileM filePath = liftIO . try $ withFile filePath ReadMode hGetContents

instance (MonadIO m, MonadReader Bootstrapped m) => MonadCommandEvaluator (ResultT m) where
  evaluate OSXReminders = evalAppleScript
  evaluate InMemory     = error . T.pack $ "should not be used yet"  -- pure . snd . eval []

instance (MonadIO m, MonadReader Bootstrapped m) => MonadLogger (ResultT m) where
  logDebug = logDebugM
  logInfo  = logInfoM
  logError = logErrorM

runResultT :: Monad m => Bootstrapped -> ResultT m a -> m (Either SyncError a)
runResultT config = usingReaderT config . runExceptT . getResultT


