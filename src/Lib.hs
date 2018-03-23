module Lib
    ( someFunc
    ) where

import Control.Monad.Free

newtype Reminder = Reminder String
  deriving (Show, Eq)

type Reminders = [Reminder]

data CommandF x = All (Reminders -> x) | Create Reminder x

instance Functor CommandF where
  fmap f (All f') = All (f . f')
  fmap f (Create r x) = Create r (f x)

type Command = Free CommandF

list :: Command Reminders
list = liftF $ All id

create :: Reminder -> Command ()
create r = liftF $ Create r ()

runTest :: Reminders -> Command x -> IO (Reminders, x)
runTest rems (Pure r) = return (rems, r)
runTest rems (Free (All f)) =  (runTest rems) . f $ rems
runTest rems (Free (Create r x)) =
  let rems' = r : rems in runTest rems' x

echo :: Command ()
echo = do create (Reminder "whatever")
          _ <- list
          create (Reminder "second")

someFunc :: IO ()
someFunc = do (r, _) <- runTest [] echo
              putStrLn (concat (show <$> r))
