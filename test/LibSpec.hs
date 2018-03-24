{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures
-Wno-unused-local-binds #-}

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

prop_CreateAdds r rs = elem r rs'
  where
    types = (r :: Reminder, rs :: Reminders)
    command = create r
    rs' = run (rs, command)

prop_CreatePersists r rs = (length rs) == (length rs') - 1
  where
    types = (r :: Reminder, rs :: Reminders)
    command = create r
    rs' = run (rs, command)

checkCommands :: IO ()
checkCommands = do
  quickCheck prop_CreateAdds
  quickCheck prop_CreatePersists
