{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module CommandSpec
  ( commandSpec
  )
where

import           Data.OrgMode.Sync.Command
import           Control.Monad.Free
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Test.Hspec
import           Test.QuickCheck
import           Data.OrgMode.Sync.Types
import           Universum               hiding ( foldl
                                                , state
                                                , first
                                                )
import           Data.List                      ( nub )

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary TodoStatus where
  arbitrary = oneof $ return <$> [Done, Todo, InProgress]

instance Arbitrary Reminder where
  arbitrary = Reminder <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

eval :: Reminders -> Command x -> (Reminders, x)
eval rems (Pure r                  ) = (rems, r)
eval rems (Free (GetAll _ f       )) = eval rems . f $ rems
eval rems (Free (CreateMany _ rs x)) = eval (rs <> rems) x
eval rems (Free (DeleteMany _ rs x)) =
  let rems' = filter (not . (`elem` rs)) rems in eval rems' x
eval rems (Free (UpdateAll _ rs x)) =
  let rest = filter (`notElem` rs) rems in eval (rs <> rest) x
eval rems (Free (ListBuckets f)) = eval rems . f $ [defaultBucket]

run :: (Reminders, Command b) -> Reminders
run = fst . uncurry eval

defaultBucket :: Bucket
defaultBucket = Bucket "1" "The List"

commandSpec :: Spec
commandSpec = describe "Synchronization commands" $ do
  describe "create command" $ do

    it "should add new todos" $ property $ \r rs ->
      let _   = (r :: Reminder, rs :: Reminders)
          rs' = run (rs, create defaultBucket r)
      in  r `notElem` rs ==> r `elem` rs'

    it "should persist existing todos" $ property $ \r rs ->
      let _   = (r :: Reminder, rs :: Reminders)
          rs' = run (rs, create defaultBucket r)
      in  r `notElem` rs ==> length rs == length rs' - 1

    it "should not duplicate existing todos" $ property $ \r rs ->
      let _    = (rs :: Reminders, r :: Reminder)
          rs'  = run (rs, create defaultBucket r)
          rs'' = run (rs', create defaultBucket r)
      in  notElem r rs ==> length rs' == length rs''

  describe "delete command" $ it "should remove todos" $ property $ \r rs ->
    let _    = (r :: Reminder, rs :: Reminders)
        rs'' = r : rs
        rs'  = run (rs'', del defaultBucket r)
    in  r `notElem` rs'

  describe "update command" $ it "should update" $ property $ \rs ->
    let uniq@(first : _) = nub rs :: Reminders
        value            = pack "special name"
        modified         = first { todoName = value }
        rs'              = run (uniq, updateMany defaultBucket [modified])
        updated          = not . null $ filter ((== value) . todoName) rs'
    in  not (null rs) ==> length uniq == length rs' && updated

  describe "synchronize command"
    $ it "synchronizes states of two lists"
    $ property
    $ \input state ->
        let _       = (input :: Reminders, state :: Reminders)
            input'  = sort input
            state'  = sort state
            state'' = run (state', sync (bucketName defaultBucket) input')
        in  input' /= state' ==> input' == sort state''