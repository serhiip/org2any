{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module LibSpec
  ( spec
  ) where

import           Command
import           Control.Monad.Free
import           Data.List          (delete, nub)
import           Data.Text          (pack)
import           Test.Hspec
import           Test.QuickCheck
import           Types

instance Arbitrary Reminder where
  arbitrary = Reminder . pack <$> arbitrary

eval :: Reminders -> Command x -> (Reminders, x)
eval rems (Pure r) = (rems, r)
eval rems (Free (All f)) = eval rems . f $ rems
eval rems (Free (Create r x)) =
  let rems' = r : rems in eval rems' x
eval rems (Free (CreateMany rs x)) =
  eval (rs ++ rems) x
eval rems (Free (Delete r x)) =
  let rems' = delete r rems in eval rems' x

run = fst . uncurry eval

spec :: Spec
spec = do
  describe "Main Algo" $ do

    describe "create" $ do
      it "should add new todos" $ property prop_CreateAdds
      it "should not duplicate existing todos" $ property prop_NotCreateExisting
      it "should persist existing todos" $ property prop_CreatePersists

    describe "del" $ do
      it "should remove todos" $ property prop_DeletesExisting

  where
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

    prop_DeletesExisting r rs = r `notElem` rs'
      where
        _ = (r :: Reminder, rs :: Reminders)
        rs'' = r : rs
        rs' = run (nub rs'', del r)
