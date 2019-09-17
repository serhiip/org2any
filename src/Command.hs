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
import qualified Data.Set                      as S
import           Types
import           Universum

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

create :: Bucket -> Reminder -> Command ()
create bid = createMany bid . S.singleton

createMany :: Bucket -> Reminders -> Command ()
createMany bid rs = liftF $ CreateMany bid rs ()

del :: Bucket -> Reminder -> Command ()
del bid = delMany bid . S.singleton

delMany :: Bucket -> Reminders -> Command ()
delMany bid rs = liftF $ DeleteMany bid rs ()

updateMany :: Bucket -> Reminders -> Command ()
updateMany bid rs = liftF $ UpdateAll bid rs ()

listBuckets :: Command Buckets
listBuckets = liftF $ ListBuckets id

sync :: Maybe BucketId -> Reminders -> Command (Either SyncError ())
sync bId toSync = do
  buckets <- listBuckets
  let name = bId ?: "Reminders"
  case
      head
        <$> (nonEmpty . S.elems . S.filter ((== name) . bucketName))
              buckets
    of
      Just bucket -> do
        existing <- list bucket
        let (updates, creations) = S.partition (`elem` existing) toSync
            deletions            = S.filter (`notElem` toSync) existing

        delMany bucket deletions
        updateMany bucket updates
        createMany bucket creations
        return $ Right ()
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
