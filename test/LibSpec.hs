{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module LibSpec
  ( spec
  ) where

import           Command
import           Control.Monad.Free
import           Data.List          (delete, find, length, lookup, nub)
import           Data.Maybe         (isJust)
import           Data.Text          (Text, pack)
import           Test.Hspec
import           Test.QuickCheck
import           Types

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Reminder where
  arbitrary = Reminder <$> arbitrary <*> arbitrary

eval :: Reminders -> Command x -> (Reminders, x)
eval rems (Pure r) = (rems, r)
eval rems (Free (All f)) = eval rems . f $ rems
eval rems (Free (Create r x)) =
  let rems' = r : rems in eval rems' x
eval rems (Free (CreateMany rs x)) =
  eval (rs ++ rems) x
eval rems (Free (Delete r x)) =
  let rems' = delete r rems in eval rems' x
eval rems (Free (UpdateAll upds x)) = eval res x
  where
    byKey = (,) <$> todoId <*> id <$> upds
    res =
      foldl
        (\acc e ->
           case lookup (todoId e) byKey of
             Just u  -> u : acc
             Nothing -> e : acc)
        []
        rems

run = fst . uncurry eval

spec :: Spec
spec =
  describe "Main Algo" $ do
    describe "create" $ do
      it "should add new todos" $ property prop_CreateAdds
      it "should not duplicate existing todos" $ property prop_NotCreateExisting
      it "should persist existing todos" $ property prop_CreatePersists
    describe "del" $ it "should remove todos" $ property prop_DeletesExisting
    describe "updateAll" $ it "should update" $ property prop_UpdateWorks
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
    -- make sure there is at least two groups with length 2 or more
    prop_UpdateWorks rs = nonZero ==> (length rs == length rs') && updated
      where
        _ = rs :: Reminders
        nonZero = not (null rs)
        first = head rs
        value = pack "special name"
        rs' = run (rs, updateAll [first {todoName = value}])
        updated = isJust $ find ((== value) . todoName) rs'
