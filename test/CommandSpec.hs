{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module CommandSpec
  ( commandSpec
  ) where

import           Control.Monad.Free             ( Free(..) )
import           Data.List                      ( nub )
import           Data.OrgMode.Sync.Command      ( create
                                                , del
                                                , sync
                                                , updateMany
                                                )
import           Data.OrgMode.Sync.Types        ( Bucket(Bucket, bucketName)
                                                , Command
                                                , CommandF
                                                  ( CreateMany
                                                  , DeleteMany
                                                  , GetAll
                                                  , ListBuckets
                                                  , UpdateAll
                                                  )
                                                , Reminder(Reminder, todoName)
                                                , Reminders
                                                , SyncError
                                                  ( InvalidDestinationError
                                                  )
                                                , TodoStatus(..)
                                                )
import           Data.Text                      ( pack )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.QuickCheck                ( (==>)
                                                , Arbitrary(arbitrary)
                                                , Testable(property)
                                                , oneof
                                                )
import           Universum

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary TodoStatus where
  arbitrary = oneof $ return <$> [Done, Todo, InProgress]

instance Arbitrary Reminder where
  arbitrary =
    Reminder
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

defaultBucket :: Bucket
defaultBucket = Bucket "1" "The List"

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

commandSpec :: Spec
commandSpec = describe "Command interpreter functionality" $ do
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

    it "should return an error if destination is invalid" $ property $ \r ->
      let _             = r :: Reminder
          invalidBucket = Bucket "2" "Invalid"
          (_, res)      = eval [] (create invalidBucket r)
      in  res == Left (InvalidDestinationError "Invalid")

  describe "delete command" $ it "should remove todos" $ property $ \r rs ->
    let _    = (r :: Reminder, rs :: Reminders)
        rs'' = r : rs
        rs'  = run (rs'', del defaultBucket r)
    in  r `notElem` rs'

  describe "update command" $ it "should update" $ property $ \rs ->
    let uniq@(first_ : _) = nub rs :: Reminders
        value             = pack "special name"
        modified          = first_ { todoName = value }
        rs'               = run (uniq, updateMany defaultBucket [modified])
        updated           = any ((== value) . todoName) rs'
    in  not (null rs) ==> length uniq == length rs' && updated

  describe "synchronize command" $ do

    it "synchronizes states of two lists" $ property $ \input state_ ->
      let _       = (input :: Reminders, state_ :: Reminders)
          input'  = sort input
          state'  = sort state_
          state'' = run (state', sync (bucketName defaultBucket) input')
      in  input' /= state' ==> input' == sort state''

    it "should return an error if destination is invalid" $ property $ \r ->
      let _             = r :: Reminder
          invalidBucket = Bucket "2" "Invalid"
          (_, res)      = eval [] (sync (bucketName invalidBucket) [r])
      in  res == Left (InvalidDestinationError "Invalid")
