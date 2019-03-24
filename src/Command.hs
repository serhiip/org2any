module Command
    ( Reminders
    , CommandF(..)
    , Command
    , runDry
    , create
    , createMany
    , list
    , del
    , updateAll
    ) where

import           Control.Monad.Free
import           Data.List          (intercalate)
import           Types

data CommandF x =
    Create Reminder x
  | All (Reminders -> x)
  | CreateMany Reminders x
  | Delete Reminder x
  | UpdateAll Reminders x

instance Functor CommandF where
  fmap f (All f')          = All (f . f')
  fmap f (Create r x)      = Create r (f x)
  fmap f (CreateMany rs x) = CreateMany rs (f x)
  fmap f (Delete r x)      = Delete r (f x)
  fmap f (UpdateAll rs x)  = UpdateAll rs (f x)

type Command = Free CommandF

list :: Command Reminders
list = liftF $ All id

create :: Reminder -> Command ()
create = createMany . pure

createMany :: Reminders -> Command ()
createMany rs = do
  existing <- list
  liftF $ CreateMany (filter (not . flip elem existing) rs) ()

del :: Reminder -> Command ()
del = liftF . flip Delete ()

updateAll :: Reminders -> Command ()
updateAll = liftF . flip UpdateAll ()

runDry :: Command x -> IO x
runDry (Pure r           ) = return r
runDry (Free (All f     )) = putStrLn "would list all" >> mempty >>= runDry . f
runDry (Free (Create r x)) = putStrLn ("would create " ++ show r) >> runDry x
runDry (Free (CreateMany rs x)) =
  putStrLn ("would create " ++ allStr) >> runDry x
  where
    allStr = intercalate ", " (show <$> rs)
runDry (Free (Delete r x)) = putStrLn ("would delete " ++ show r) >> runDry x
runDry (Free (UpdateAll rs x)) = putStrLn ("would delete " ++ show rs) >> runDry x
