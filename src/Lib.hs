module Lib
  ( Reminder(Reminder)
  , Reminders
  , CommandF(All, Create)
  , Command
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
create r = liftF $ Create r ()
