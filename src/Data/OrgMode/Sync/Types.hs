{-|
Module      : Data.OrgMode.Sync.Types
Description : Definitions representing common datatypes
License     : GPL-3
Maintainer  : Serhii <serhii@proximala.bz>
Stability   : experimental

Reminder item internal represenatation, main transformer stack and
 configuration datatypes.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.OrgMode.Sync.Types
  ( Reminder(..)
  , Reminders
  , BucketId
  , Buckets
  , TodoStatus(..)
  , Event(..)
  , ActionType(..)
  , SyncConfig(..)
  , Verbosity(..)
  , SyncError(..)
  , Bootstrapped(..)
  , Bucket(..)
  , OrgLike(..)
  , MonadFileReader(..)
  , Command
  , CommandF(..)
  , MonadCommandEvaluator(..)
  , StoreType(..)
  ) where

import           Control.Concurrent.Chan        ( Chan )
import           Control.Exception              ( IOException )
import           Control.Monad.Free             ( Free )
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
  = UserTerminatedEvent Text
  -- ^ User requested the program termination
  | SystemTerminatedEvent
  -- ^ SIGINT or SIGTERM received from OS
  | SyncEvent FilePath (Maybe Text)
  -- ^ User request to synchronize a file
  | EndEvent
  -- ^ Signal main thread that programm could be stopped - all request
  -- are fulfiled and all resources are claned up
  deriving (Show, Eq)

-- | Action that can be performed by the system
data ActionType = SyncAction FilePath (Maybe Text)
  -- ^ File synchronization action
  deriving (Show, Eq)

-- | Configuration of how synchronization should be done,
-- supplied by the user via CLI arguments
data SyncConfig = SyncConfig
  { configVerbosity :: Verbosity
      -- ^ How much information to send to stdout / stderr
  }
  deriving Show

-- | Bootstrapped program configuration
data Bootstrapped = Bootstrapped
  { bootstrappedConfig  :: SyncConfig
  -- ^ The configuration supplied by user (unmodified)
  , bootstrappedLoggers :: (TimedFastLogger, TimedFastLogger)
  -- ^ stdout / stderr loggers initialized
  , bootstrappedInput   :: Chan Event
  -- ^ Input channel
  , bootstrappedOutput  :: Chan ()
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
  | ParseError FilePath
  -- ^ Error parsing org file
  deriving (Show, Eq)

instance ToLogStr SyncError where
  toLogStr (SysCallError bs) = toLogStr bs
  toLogStr (NoItemsError path) =
    toLogStr $ "No org items found to import in " <> toText path
  toLogStr (InvalidDestinationError destination) =
    toLogStr
      $  "There was an error getting reminders from "
      <> destination
      <> ". Try specifying different name"
  toLogStr (DecodeError raw err) =
    toLogStr $ "Error decoding output " <> toText raw <> " got error " <> toText err
  toLogStr (FileReadError path original) =
    toLogStr $ "There was an error reading " <> toText path <> " got error " <> original

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
  { todoName       :: Text
  , todoId         :: Text
  , todoBody       :: Maybe Text
  , todoStatus     :: Maybe TodoStatus
  , todoOriginalId :: Text
  -- ^ The ID in the destination it was created from (for org file
  -- @todoId == todoOriginalId@)
  }
  deriving Show

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
data Bucket = Bucket
  { bucketId   :: BucketId
  , bucketName :: Text
  }
  deriving (Show, Eq, Ord)

-- | The type of program or service that would provide or store items
data StoreType = InMemory | OSXReminders deriving (Show, Eq, Ord)

-- | Alias for list of places reminders could be stored in
type Buckets = [Bucket]

-- | Allows to convert back and forth between various reminder
-- representations. Allows to abstract @Command@ interpreter from
-- concrete implementation of reminder by converting TODO items on the
-- fly
class OrgLike a where
  from :: a -> Maybe Reminder
  -- ^ Convert some representation to internal representation
  to :: Reminder -> a
  -- ^ Convert internal representation to some other TODO
  -- representation

instance OrgLike Reminder where
  from = pure
  to   = id

-- | An action that could be performed. All of the action work against
-- particular `Data.OrgMode.Sync.Types.Bucket`
data CommandF x
  = GetAll Bucket (Reminders -> x)
  -- ^ List all reminders
  | CreateMany Bucket Reminders x
  -- ^ Create some reminders
  | DeleteMany Bucket Reminders x
  -- ^ Delete some reminders
  | UpdateAll Bucket Reminders x
  -- ^ Update some reminders
  | ListBuckets (Buckets -> x)
  -- ^ List available buckets of reminders
  deriving (Functor)

-- | A description of a programm detached from its implementation
type Command = Free CommandF

class Monad m => MonadFileReader m where
  readFileM :: FilePath -> m (Either IOException Text)

class Monad m => MonadCommandEvaluator m where
   evaluate :: StoreType -> Command x -> m x
