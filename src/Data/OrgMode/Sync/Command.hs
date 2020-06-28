{-|
Module      : Data.OrgMode.Sync.Command
Description : Synchronization scripts as `Data.Control.Free` monads
License     : GPL-3
Maintainer  : Serhii <serhii@proximala.bz>
Stability   : experimental

Utilities to create manipulation scripts without knowledge of the
 destination against which constructed script will be executed.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Data.OrgMode.Sync.Command
  ( Reminders
  , CommandF(..)
  , Command
  , runDry
  , sync
  , listBuckets
  , create
  , del
  , updateMany
  )
where

import           Control.Monad.Free
import           Data.OrgMode.Sync.Types
import           Universum               hiding ( elem
                                                , notElem
                                                )
import           Data.List                      ( partition
                                                , elem
                                                , notElem
                                                )

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

instance Functor CommandF where
  fmap f (GetAll bid f'      ) = GetAll bid (f . f')
  fmap f (CreateMany bid rs x) = CreateMany bid rs (f x)
  fmap f (DeleteMany bid rs x) = DeleteMany bid rs (f x)
  fmap f (UpdateAll  bid rs x) = UpdateAll bid rs (f x)
  fmap f (ListBuckets f'     ) = ListBuckets (f . f')

-- | A description of a programm detached from its implementation
type Command = Free CommandF

-- | Do nothing
noOp :: Monad m => Command (m ())
noOp = return $ pure ()

-- | Construct a part of a script that lists all available reminders
list :: Bucket -> Command Reminders
list bid = liftF $ GetAll bid id

-- | Construct a part of a script to create one riminder item
create :: OrgLike a => Bucket -> a -> Command (Either SyncError ())
create bid (from -> Just reminder) = createMany bid . pure $ reminder
create _   _                       = noOp

-- | Construct a part of a script to cteate some reminders
createMany :: OrgLike a => Bucket -> [a] -> Command (Either SyncError ())
createMany bid@(Bucket _ name) rs = do
  buckets <- listBuckets
  case head <$> (nonEmpty . filter ((== name) . bucketName)) buckets of
    Just bucket -> do
      existing <- list bucket
      let toCreate = filter (`notElem` existing) (mapMaybe from rs)
      liftF $ Right <$> CreateMany bid toCreate ()
    Nothing -> return . Left . InvalidDestinationError $ name

-- | Construct part of a programm to delete one reminder
del :: OrgLike a => Bucket -> a -> Command ()
del bid r = delMany bid [r]

-- | Construct part of a porgramm to delete some reminders
delMany :: OrgLike a => Bucket -> [a] -> Command ()
delMany bid rs = liftF $ DeleteMany bid (mapMaybe from rs) ()

-- | Construct part of a programm to update some reminders
updateMany :: OrgLike a => Bucket -> [a] -> Command ()
updateMany bid rs = liftF $ UpdateAll bid (mapMaybe from rs) ()

-- | Construct part of a programm that would list all available buckets
-- (reminder lists) 
listBuckets :: Command Buckets
listBuckets = liftF $ ListBuckets id

-- | Construct syncronization part of a programm.
-- The synchronization procedure consists of following steps:
--
--  1. List all of the buckets and check that destination bucket
--     actually exists
--
--  2. List all of the existing reminders within that bucket and
--     derive lists of items that should be deleted, updated and
--     created
--
--  3. Delete reminders
--
--  4. Update reimnders
--
--  5. Create new reminders
sync :: OrgLike a => BucketId -> [a] -> Command (Either SyncError ())
sync name toSync = do
  buckets <- listBuckets
  case head <$> (nonEmpty . filter ((== name) . bucketName)) buckets of
    Just bucket -> do
      existing <- list bucket
      let toSync'              = mapMaybe from toSync
          (updates, creations) = partition (`elem` existing) toSync'
          deletions            = filter (`notElem` toSync') existing

      delMany bucket deletions
      updateMany bucket updates
      createMany bucket creations
    Nothing -> return . Left . InvalidDestinationError $ name

-- | An interpreter for `Data.OrgMode.Sync.Command` that just prints
-- the steps to be performed to stdout (a.k.a. dry run). Has a caveat
-- in regards the fact that the actually existing reminders are never
-- listed - so only steps 1, 2 and 5 are executed in `sync`
runDry :: Command x -> IO x
runDry (Pure r) = return r
runDry (Free (GetAll bid f)) =
  putStrLn @String ("would list all in " <> show bid) >> mempty >>= runDry . f
runDry (Free (CreateMany bid rs rest)) =
  putStrLn @String ("would create " <> show rs <> " in " <> show bid) >> runDry rest
runDry (Free (DeleteMany bid rs rest)) =
  putStrLn @String ("would delete " <> show rs <> " in " <> show bid) >> runDry rest
runDry (Free (UpdateAll bid rs rest)) =
  putStrLn @String ("would delete " <> show rs <> " in " <> show bid) >> runDry rest
runDry (Free (ListBuckets rest)) =
  putStrLn @String "would list all todo lists" >> mempty >>= runDry . rest
