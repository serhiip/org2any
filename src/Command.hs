module Command
  ( Reminders
  , CommandF(..)
  , Command
  , runDry
  , create
  , createMany
  , list
  ) where

import           Control.Monad.Free
import           Data.List          (intercalate)
import           Types

data CommandF x =
    Create Reminder x
  | All (Reminders -> x)
  | CreateMany Reminders x

instance Functor CommandF where
  fmap f (All f')          = All (f . f')
  fmap f (Create r x)      = Create r (f x)
  fmap f (CreateMany rs x) = CreateMany rs (f x)

type Command = Free CommandF

list :: Command Reminders
list = liftF $ All id

create :: Reminder -> Command ()
create = createMany . pure

createMany :: Reminders -> Command ()
createMany rs = do
  existing <- list
  liftF $ CreateMany (filter (not . flip elem existing) rs) ()

runDry :: Command x -> IO x
runDry (Pure r) = return r
runDry (Free (All f)) = putStrLn "would list all" >> mempty >>= runDry . f
runDry (Free (Create r x)) = putStrLn ("would create " ++ show r) >> runDry x
runDry (Free (CreateMany rs x)) =
  putStrLn ("would create all " ++ intercalate ", " (show <$> rs)) >> runDry x
