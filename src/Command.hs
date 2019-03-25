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
    ) where

import           Control.Monad.Free
import           Data.Set
import           Prelude            hiding (filter)
import           Types

data CommandF x =
  All (Reminders -> x)
  | CreateMany Reminders x
  | DeleteMany Reminders x
  | UpdateAll Reminders x

instance Functor CommandF where
  fmap f (All f')          = All (f . f')
  fmap f (CreateMany rs x) = CreateMany rs (f x)
  fmap f (DeleteMany rs x) = DeleteMany rs (f x)
  fmap f (UpdateAll rs x)  = UpdateAll rs (f x)

type Command = Free CommandF

list :: Command Reminders
list = liftF $ All id

create :: Reminder -> Command ()
create = createMany . singleton

createMany :: Reminders -> Command ()
createMany rs = do
  existing <- list
  liftF $ CreateMany (filter (not . flip elem existing) rs) ()

del :: Reminder -> Command ()
del = delMany . singleton

delMany :: Reminders -> Command ()
delMany = liftF . flip DeleteMany ()

updateMany :: Reminders -> Command ()
updateMany = liftF . flip UpdateAll ()

sync :: Reminders -> Command ()
sync toSync = do
  existing <- list
  let (updates, creations) = partition (`elem` existing) toSync
      deletions = filter (`notElem` toSync) existing

  updateMany updates
  delMany deletions
  createMany creations
  pure ()

runDry :: Command x -> IO x
runDry (Pure r           ) = return r
runDry (Free (All f     )) = putStrLn "would list all" >> mempty >>= runDry . f
runDry (Free (CreateMany rs x)) = putStrLn ("would create " ++ show rs) >> runDry x
runDry (Free (DeleteMany rs x)) = putStrLn ("would delete " ++ show rs) >> runDry x
runDry (Free (UpdateAll rs x)) = putStrLn ("would delete " ++ show rs) >> runDry x

