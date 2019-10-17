{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Command
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
import           Types
import           Universum               hiding ( elem
                                                , notElem
                                                )
import           Data.List                      ( partition
                                                , elem
                                                , notElem
                                                , filter
                                                )

data CommandF x =
    GetAll Bucket (Reminders -> x)
  | CreateMany Bucket Reminders x
  | DeleteMany Bucket Reminders x
  | UpdateAll Bucket Reminders x
  | ListBuckets (Buckets -> x)

instance Functor CommandF where
  fmap f (GetAll bid f') = GetAll bid (f . f')
  fmap f (CreateMany bid rs x) = CreateMany bid rs (f x)
  fmap f (DeleteMany bid rs x) = DeleteMany bid rs (f x)
  fmap f (UpdateAll bid rs x) = UpdateAll bid rs (f x)
  fmap f (ListBuckets f') = ListBuckets (f . f')

type Command = Free CommandF

list :: Bucket -> Command Reminders
list bid = liftF $ GetAll bid id

create :: OrgLike a => Bucket -> a -> Command (Either SyncError ())
create bid = createMany bid . pure . from

createMany :: OrgLike a => Bucket -> [a] -> Command (Either SyncError ())
createMany bid@(Bucket _ name) rs = do
  buckets <- listBuckets
  case head <$> (nonEmpty . filter ((== name) . bucketName)) buckets of
    Just bucket -> do
      existing <- list bucket
      let toCreate = filter (`notElem` existing) (from <$> rs)
      liftF $ Right <$> CreateMany bid toCreate ()
    Nothing -> return . Left . InvalidDestinationError $ name

del :: OrgLike a => Bucket -> a -> Command ()
del bid r = delMany bid [r]

delMany :: OrgLike a => Bucket -> [a] -> Command ()
delMany bid rs = liftF $ DeleteMany bid (from <$> rs) ()

updateMany :: OrgLike a => Bucket -> [a] -> Command ()
updateMany bid rs = liftF $ UpdateAll bid (from <$> rs) ()

listBuckets :: Command Buckets
listBuckets = liftF $ ListBuckets id

sync :: OrgLike a => BucketId -> [a] -> Command (Either SyncError ())
sync name toSync = do
  buckets <- listBuckets
  case head <$> (nonEmpty . filter ((== name) . bucketName)) buckets of
    Just bucket -> do
      existing <- list bucket
      let toSync'              = from <$> toSync
          (updates, creations) = partition (`elem` existing) toSync'
          deletions            = filter (`notElem` toSync') existing

      delMany bucket deletions
      updateMany bucket updates
      createMany bucket creations
    Nothing -> return . Left . InvalidDestinationError $ name

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
