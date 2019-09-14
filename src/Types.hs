{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( Reminder(..)
  , Reminders
  , BucketId
  , Buckets
  , TodoStatus(..)
  , Event(..)
  , remindersFromList
  , remindersToList
  , remindersToMapping
  , O2AM(..)
  , runO2AM
  , SyncConfig(..)
  , Verbosity(..)
  , SyncError(..)
  , Bootstrapped(..)
  , Bucket(..)
  )
where

import           Control.Concurrent.Chan        ( Chan )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Function                  ( on )
import qualified Data.Map.Strict               as MS
import           Data.Set                       ( fromList
                                                , Set
                                                )
import qualified Data.Text                     as T
import           System.FilePath                ( FilePath )
import           System.Log.FastLogger          ( TimedFastLogger )
import           Universum

data Verbosity = Normal | Verbose | Quiet deriving (Show, Eq)

data Event =
    UserTerminatedEvent T.Text
  | SystemTerminatedEvent
  | SyncEvent FilePath (Maybe Text)
  | EndEvent
  deriving (Show, Eq)

data SyncConfig = SyncConfig
      { configVerbosity :: Verbosity
      } deriving (Show)

data Bootstrapped = Bootstrapped
  { bootstrappedConfig :: SyncConfig
  , bootstrappedLoggers :: (TimedFastLogger, TimedFastLogger)
  , bootstrappedInput :: Chan Event
  , bootstrappedOutput :: Chan ()
  }

data SyncError = SysCallError Text | NoItemsError FilePath | InvalidDestinationError Text deriving (Show)

data TodoStatus
  = Todo
  | Done
  | InProgress
  deriving (Show, Eq, Ord)

-- | Main representation of TODO item
data Reminder = Reminder
  { todoName   :: T.Text
  , todoId     :: T.Text
  , todoBody   :: T.Text
  , todoStatus :: Maybe TodoStatus
  } deriving (Show)

instance Eq Reminder where
  (==) = (==) `on` todoId

instance Ord Reminder where
  compare = compare `on` todoId

type Reminders = Set Reminder

type BucketId = Text

data Bucket = Bucket { bucketId :: BucketId
                     , bucketName :: Text
                     } deriving (Show, Eq, Ord)

type Buckets = Set Bucket

remindersFromList :: [Reminder] -> Reminders
remindersFromList = fromList

remindersToList :: Reminders -> [Reminder]
remindersToList = toList @(Set Reminder)

remindersToMapping :: Reminders -> MS.Map T.Text Reminder
remindersToMapping rems =
  MS.fromList $ (,) <$> todoId <*> id <$> toList @(Set Reminder) rems

newtype O2AM a = O2AM
  { getO2AM :: ExceptT SyncError (ReaderT Bootstrapped IO) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader Bootstrapped
  , MonadError SyncError
  , MonadThrow
  , MonadCatch
  )

runO2AM :: Bootstrapped -> O2AM a -> IO (Either SyncError a)
runO2AM config = usingReaderT config . runExceptT . getO2AM
