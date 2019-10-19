{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( Reminder(..)
  , Reminders
  , BucketId
  , Buckets
  , TodoStatus(..)
  , Event(..)
  , remindersToMapping
  , Result
  , UnitResult
  , runResult
  , SyncConfig(..)
  , Verbosity(..)
  , SyncError(..)
  , Bootstrapped(..)
  , Bucket(..)
  , OrgLike(..)
  )
where

import           Control.Concurrent.Chan        ( Chan )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Function                  ( on )
import qualified Data.Map.Strict               as MS
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

data SyncError =
    SysCallError Text
  | NoItemsError FilePath
  | InvalidDestinationError Text
  | DecodeError Text Text deriving (Show)

data TodoStatus
  = Todo
  | Done
  | InProgress
  deriving (Show, Eq, Ord)

-- | Main representation of TODO item
data Reminder = Reminder
  { todoName   :: T.Text
  , todoId     :: T.Text
  , todoBody   :: Maybe T.Text
  , todoStatus :: Maybe TodoStatus
  } deriving (Show)

instance Eq Reminder where
  (==) = (==) `on` todoId

instance Ord Reminder where
  compare = compare `on` todoId

type Reminders = [Reminder]

type BucketId = Text

data Bucket = Bucket { bucketId :: BucketId
                     , bucketName :: Text
                     } deriving (Show, Eq, Ord)

type Buckets = [Bucket]

remindersToMapping :: Reminders -> MS.Map T.Text Reminder
remindersToMapping rems = MS.fromList $ (,) <$> todoId <*> id <$> rems

class OrgLike a where
  from :: a -> Reminder
  to :: Reminder -> a

instance OrgLike Reminder where
  from = id
  to = id

newtype ResultT m a = O2AMT
  { getResultT :: ExceptT SyncError (ReaderT Bootstrapped m) a
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

runResultT :: Monad m => Bootstrapped -> ResultT m a -> m (Either SyncError a)
runResultT config = usingReaderT config . runExceptT . getResultT

type Result = ResultT IO
type UnitResult = Result ()

runResult :: Bootstrapped -> Result a -> IO (Either SyncError a)
runResult = runResultT
