{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module LibSpec
  ( checkCommands
  ) where

import           Control.Monad.Free
import           Command
import           Test.QuickCheck
import           Data.Text            (pack)

instance Arbitrary Reminder where
  arbitrary = Reminder . pack <$> arbitrary

runTest :: Reminders -> Command x -> (Reminders, x)
runTest rems (Pure r) = (rems, r)
runTest rems (Free (All f)) = runTest rems . f $ rems
runTest rems (Free (Create r x)) =
  let rems' = r : rems in runTest rems' x

run = fst . uncurry runTest

prop_CreateAdds r rs = r `notElem` rs ==> elem r rs'
  where
    _ = (r :: Reminder, rs :: Reminders)
    command = create r
    rs' = run (rs, command)

prop_CreatePersists r rs = r `notElem` rs ==> length rs == length rs' - 1
  where
    _ = (r :: Reminder, rs :: Reminders)
    rs' = run (rs, create r)

prop_NotCreateExisting r rs = r `notElem` rs ==> length rs' == length rs''
  where
    _ = (r :: Reminder, rs :: Reminders)
    rs'' = r : rs
    rs' = run (rs'', create r)

checkCommands :: IO ()
checkCommands = do
  quickCheck prop_CreateAdds
  quickCheck prop_CreatePersists
  quickCheck prop_NotCreateExisting
