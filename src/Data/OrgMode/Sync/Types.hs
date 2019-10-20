{-|
Module      : Data.OrgMode.Sync.Types
Description : Definitions representing common datatypes
License     : GPL-3
Maintainer  : Serhii <serhii@proximala.bz>
Stability   : experimental

Reminder item internal represenatation, main transformer stack and
 configuration datatypes.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Sync.Types
  ( Reminder(..)
  , Reminders
  , BucketId
  , Buckets
  , TodoStatus(..)
  , Event(..)
  , Result
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
import qualified Data.Text                     as T
import           System.FilePath                ( FilePath )
import           System.Log.FastLogger          ( TimedFastLogger
                                                , ToLogStr(..)
                                                )
import           Universum

-- | The amount of logging output 
data Verbosity
  = Normal
  | Verbose
  | Quiet
  deriving (Show, Eq)

-- | Events coming from outside world (user or system)
data Event
  = UserTerminatedEvent T.Text
  -- ^ User requested the program termination
  | SystemTerminatedEvent
  -- ^ SIGINT or SIGTERM received from OS
  | SyncEvent FilePath (Maybe Text)
  -- ^ User request to synchronize a file
  | EndEvent
  -- ^ Signal main thread that programm could be stopped - all request
  -- are fulfiled and all resources are claned up
  deriving (Show, Eq)

-- | Configuration of how synchronization should be done,
-- supplied by the user via CLI arguments
data SyncConfig = SyncConfig
      { configVerbosity :: Verbosity
      -- ^ How much information to send to stdout / stderr
      } deriving (Show)

-- | Bootstrapped program configuration
data Bootstrapped = Bootstrapped
  { bootstrappedConfig :: SyncConfig
  -- ^ The configuration supplied by user (unmodified)
  , bootstrappedLoggers :: (TimedFastLogger, TimedFastLogger)
  -- ^ stdout / stderr loggers initialized
  , bootstrappedInput :: Chan Event
  -- ^ Input channel
  , bootstrappedOutput :: Chan ()
  -- ^ Output channel (just to flag program termination to main
  -- thread)
  }

-- | Representation of errors that could happen during program
-- execution
data SyncError
  = SysCallError Text
  -- ^ Error calling some system utility (only __osascript__ at this
  -- point)
  | NoItemsError FilePath
  -- ^ When there are no items in org file found passed as argument
  | InvalidDestinationError Text
  -- ^ When the destination of synchronization is invalid. In case of
  -- Reminders OS X app this means the list name is not valid or there
  -- is no default list name for some reason
  | DecodeError Text Text
  -- ^ Error decoding output from __osascript__. Unlikely to happen
  -- but usefull to see during development
  | FileReadError FilePath Text
  -- ^ Error reading input file
  deriving (Show)

instance ToLogStr SyncError where
  toLogStr (SysCallError bs) = toLogStr bs
  toLogStr (NoItemsError path) =
    toLogStr $ "No org items found to import in " <> toText path
  toLogStr (InvalidDestinationError destination) =
    toLogStr $ "There was an error getting reminders from "
    <> T.unpack destination
    <> ". Try specifying different name"
  toLogStr (DecodeError raw err) = toLogStr $
    "Error decoding output "
    <> toText raw
    <> " got error "
    <> toText err
  toLogStr (FileReadError path original) = toLogStr $
    "There was an error reading "
    <> toText path
    <> " got error "
    <> original

-- | Status keyword for headlines in org file. Has no support for
-- custom status keywords yet
data TodoStatus
  = Todo
  | Done
  | InProgress
  deriving (Show, Eq, Ord)

-- | Internal representation of org-like items. Usefull as an
-- intermediate type of various types of reminders (at this point only
-- org headlines or items of Reminders OS X application)
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

-- | Synonym for the list of reminder items
type Reminders = [Reminder]

-- | Identifier of places (simply TODO list names) of storage of
-- reminder items
type BucketId = Text

-- | Information of place the reminders could be stored in. For
-- Reminders OS X application that is simply list name and ID. But for
-- org file that could be org file name
data Bucket = Bucket { bucketId :: BucketId
                     , bucketName :: Text
                     } deriving (Show, Eq, Ord)

-- | Alias for list of places reminders could be stored in
type Buckets = [Bucket]

-- | Allows to convert back and forth between various reminder
-- representations. Allows to abstract @Command@ interpreter from
-- concrete implementation of reminder by converting TODO items on the
-- fly
class OrgLike a where
  from :: a -> Reminder
  -- ^ Convert some representation to internal representation
  to :: Reminder -> a
  -- ^ Convert internal representation to some other TODO
  -- representation

instance OrgLike Reminder where
  from = id
  to = id

-- | Main transformers stack. Allows to do IO, monadic computation,
-- have common context (via Reader) and work with pure exceptions
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
  , MonadMask
  )

runResultT :: Monad m => Bootstrapped -> ResultT m a -> m (Either SyncError a)
runResultT config = usingReaderT config . runExceptT . getResultT

-- | Execute trasformer stack
runResult :: Bootstrapped -> Result a -> IO (Either SyncError a)
runResult = runResultT

-- | Yield result requiring some side effects to be executed.  ResultT
-- is `newtype` alias to a set of transformers derrived using
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-GeneralizedNewtypeDeriving GeneralizedNewtypeDeriving>
-- extension
type Result = ResultT IO
