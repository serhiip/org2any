{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}

module LibSpec
  ( spec
  )
where

import           Command
import           Control.Monad.Free
import           Data.Set                       ( elemAt
                                                , filter
                                                , insert
                                                , null
                                                , singleton
                                                , union
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Prelude                 hiding ( filter
                                                , foldl
                                                , null
                                                )
import           Test.Hspec
import           Test.QuickCheck
import           Types

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary TodoStatus where
  arbitrary = oneof $ return <$> [Done, Todo, InProgress]

instance Arbitrary Reminder where
  arbitrary = Reminder <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

eval :: Reminders -> Command x -> (Reminders, x)
eval rems (Pure r              ) = (rems, r)
eval rems (Free (GetAll f)     ) = eval rems . f $ rems
eval rems (Free CreateMany {..}) = eval (rs <> rems) x
eval rems (Free DeleteMany {..}) = let rems' = filter (not . (`elem` rs)) rems in eval rems' x
eval rems (Free UpdateAll {..} ) = eval (rs `union` rems) x

run = fst . uncurry eval

spec :: Spec
spec = describe "Commands" $ do
  describe "create" $ do

    it "should add new todos" $ property $ \r rs ->
      let _   = (r :: Reminder, rs :: Reminders)
          rs' = run (rs, create r)
      in  r `notElem` rs ==> r `elem` rs'

    it "should persist existing todos" $ property $ \r rs ->
      let _   = (r :: Reminder, rs :: Reminders)
          rs' = run (rs, create r)
      in  r `notElem` rs ==> length rs == length rs' - 1

    it "should not duplicate existing todos" $ property $ \r rs ->
      let _    = (r :: Reminder, rs :: Reminders)
          rs'' = insert r rs
          rs'  = run (rs'', create r)
      in  r `notElem` rs ==> length rs' == length rs''

  describe "del" $ it "should remove todos" $ property $ \r rs ->
    let _    = (r :: Reminder, rs :: Reminders)
        rs'' = insert r rs
        rs'  = run (rs'', del r)
    in  r `notElem` rs'

  describe "update" $ it "should update" $ property $ \rs ->
    let _       = rs :: Reminders
        nonZero = not (null rs)
        first   = elemAt 0 rs
        value   = pack "special name"
        rs'     = run (rs, updateMany (singleton first { todoName = value }))
        updated = not . null $ filter ((== value) . todoName) rs'
    in  nonZero ==> (length rs == length rs') && updated

  describe "sync" $ it "sync states of two lists" $ property $ \input state ->
    let _      = (input :: Reminders, state :: Reminders)
        state' = run (state, sync input)
    in  input /= state ==> input == state'
