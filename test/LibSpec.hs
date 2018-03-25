{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module LibSpec
  ( checkCommands
  ) where

import           Control.Monad
import           Control.Monad.Free
import           Lib
import           Test.QuickCheck

instance Arbitrary Reminder where
  arbitrary = liftM Reminder arbitrary

runTest :: Reminders -> Command x -> (Reminders, x)
runTest rems (Pure r) = (rems, r)
runTest rems (Free (All f)) = (runTest rems) . f $ rems
runTest rems (Free (Create r x)) =
  let rems' = r : rems in runTest rems' x

run = fst . (uncurry runTest)

prop_CreateAdds r rs = not (elem r rs) ==> elem r rs'
  where
    _ = (r :: Reminder, rs :: Reminders)
    command = create r
    rs' = run (rs, command)

prop_CreatePersists r rs = not (elem r rs) ==> (length rs) == (length rs') - 1
  where
    _ = (r :: Reminder, rs :: Reminders)
    command = create r
    rs' = run (rs, command)

prop_NotCreateExisting r rs = not (elem r rs) ==> (length rs') == (length rs'')
  where
    _ = (r :: Reminder, rs :: Reminders)
    rs'' = r : rs
    rs' = run (rs'', create r)

checkCommands :: IO ()
checkCommands = do
  quickCheck prop_CreateAdds
  quickCheck prop_CreatePersists
  quickCheck prop_NotCreateExisting
