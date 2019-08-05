{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Command
  ( Reminders
  , CommandF(..)
  , Command
  , runDry
  , create
  , createMany
  , list
  , del
  , updateMany
  , sync
  )
where

import           Universum
import           Control.Monad.Free
import qualified Data.Set                      as S
import           Types

data CommandF x =
  GetAll {cont :: Reminders -> x}
  | CreateMany {rs :: Reminders, x :: x}
  | DeleteMany {rs :: Reminders, x :: x}
  | UpdateAll {rs :: Reminders, x :: x}

instance Functor CommandF where
  fmap f (GetAll f')          = GetAll (f . f')
  fmap f CreateMany {..} = CreateMany rs (f x)
  fmap f DeleteMany {..} = DeleteMany rs (f x)
  fmap f UpdateAll {..}  = UpdateAll rs (f x)

type Command = Free CommandF

list :: Command Reminders
list = liftF $ GetAll id

create :: Reminder -> Command ()
create = createMany . S.singleton

createMany :: Reminders -> Command ()
createMany rs = do
  existing <- list
  liftF $ CreateMany (S.filter (not . flip elem existing) rs) ()

del :: Reminder -> Command ()
del = delMany . S.singleton

delMany :: Reminders -> Command ()
delMany = liftF . flip DeleteMany ()

updateMany :: Reminders -> Command ()
updateMany = liftF . flip UpdateAll ()

sync :: Reminders -> Command ()
sync toSync = do
  existing <- list
  let (updates, creations) = S.partition (`elem` existing) toSync
      deletions            = S.filter (`notElem` toSync) existing

  delMany deletions
  updateMany updates
  createMany creations
  pure ()

runDry :: Command x -> IO x
runDry (Pure r                ) = return r
runDry (Free (GetAll f       )) = putStrLn "would list all" >> mempty >>= runDry . f
runDry (Free (CreateMany rs x)) = putStrLn ("would create " ++ show rs) >> runDry x
runDry (Free (DeleteMany rs x)) = putStrLn ("would delete " ++ show rs) >> runDry x
runDry (Free (UpdateAll  rs x)) = putStrLn ("would delete " ++ show rs) >> runDry x
