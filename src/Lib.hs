module Lib
  ( Reminder(Reminder)
  , Reminders
  , CommandF(All, Create)
  , Command
  , runDry
  , create
  , list
  ) where

import           Control.Monad.Free

newtype Reminder = Reminder String
  deriving (Show, Eq)

type Reminders = [Reminder]

data CommandF x =
    Create Reminder x
  | All (Reminders -> x)

instance Functor CommandF where
  fmap f (All f')     = All (f . f')
  fmap f (Create r x) = Create r (f x)

type Command = Free CommandF

list :: Command Reminders
list = liftF $ All id

create :: Reminder -> Command ()
create r = do
  existing <- list
  if (elem r existing)
    then Pure ()
    else liftF $ Create r ()



runDry :: Command x -> IO x
runDry (Pure r) = return r
runDry (Free (All f)) = putStrLn "would list all" >> mempty >>= runDry . f
runDry (Free (Create r x)) = putStrLn ("would create " ++ (show r)) >> runDry x
