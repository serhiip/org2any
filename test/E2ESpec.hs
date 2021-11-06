{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module E2ESpec
  ( e2eSpec
  ) where

import           Test.Hspec                     ( Expectation
                                                , Spec
                                                , SpecWith
                                                , before
                                                , describe
                                                , it
                                                , shouldBe
                                                , xit
                                                )
import           Universum

import           Control.Exception              ( IOException )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.RWS              ( MonadWriter(..)
                                                , RWST(..)
                                                , execRWST
                                                )
import qualified Data.List                     as L
import           Data.OrgMode.Sync.Command      ( eval )
import           Data.OrgMode.Sync.Executor     ( handle )
import           Data.OrgMode.Sync.Logging      ( MonadLogger(..) )
import           Data.OrgMode.Sync.Types        ( ActionType(..)
                                                , Bootstrapped(..)
                                                , MonadCommandEvaluator(..)
                                                , MonadFileReader(..)
                                                , Reminder(..)
                                                , SyncConfig(..)
                                                , SyncError(..)
                                                , Verbosity(..)
                                                )
import qualified Data.Text                     as T
import           GHC.IO.Exception               ( IOErrorType(NoSuchThing)
                                                , IOException(IOError)
                                                )
import qualified Prelude
import           System.Log.FastLogger          ( LogStr
                                                , fromLogStr
                                                , toLogStr
                                                )
import qualified Universum.String.Conversion   as E

data TestState = TestState
  { inputFileProvider :: FilePath -> Either IOException T.Text
  , reminders         :: [Reminder]
  }

data TestOutput = TestOutput
  { debugLogs :: [LogStr]
  , infoLogs  :: [LogStr]
  , errorLogs :: [LogStr]
  }
  deriving Show

instance Semigroup TestOutput where
  TestOutput d i e <> TestOutput d' i' e' = TestOutput (d <> d') (i <> i') (e <> e')

instance Monoid TestOutput where
  mempty = TestOutput mempty mempty mempty

instance Prelude.Show TestState where
  show = mappend "TestState " . show . reminders

-- | Main transformers stack for testing
newtype TestResultT m a = TestO2AMT
  { getTestResultT :: RWST Bootstrapped TestOutput TestState (ExceptT SyncError m) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader Bootstrapped
  , MonadState TestState
  , MonadError SyncError
  , MonadWriter TestOutput
  , MonadThrow
  , MonadCatch
  , MonadMask
  )

-- | Aggregate the logs instead of writing to output
instance Monad m => MonadLogger (TestResultT m) where
  logDebug msg = tell $ TestOutput [toLogStr msg] mempty mempty
  logInfo msg = tell $ TestOutput mempty [toLogStr msg] mempty
  logError msg = tell $ TestOutput mempty mempty [toLogStr msg]

-- | Read a file from test state instead of file system
instance Monad m => MonadFileReader (TestResultT m) where
  readFileM p = inputFileProvider <$> get <*> pure p

-- | Evaluate command purely and write the result to special state field
instance Monad m => MonadCommandEvaluator (TestResultT m) where
  evaluate _ command = do
    existingReminders <- gets reminders
    let (newReminders, result) = eval existingReminders command

    modify (\st -> st { reminders = newReminders })
    return result

defaultBootstrappedConfig :: Bootstrapped
defaultBootstrappedConfig = Bootstrapped
  (SyncConfig Verbose)
  (error "should not access loggers in tests")
  (error "should not access input channel in tests")
  (error "should not access output channel in tests")

runTestResultT
  :: Monad m
  => Bootstrapped
  -> Maybe Text
  -> ActionType
  -> m (Either SyncError (TestState, TestOutput))
runTestResultT config contents action =
  let initialState = case contents of
        Just txt -> TestState (const . Right $ txt) []
        Nothing  -> TestState readFileHandler []
  in  runExceptT . (flip . flip execRWST) config initialState . getTestResultT $ handle
        action
 where
  readFileHandler fileName = case fileName of
    "test.org"  -> Right "* this is parent item\n**this is child item"
    "empty.org" -> Right ""
    _           -> Left $ IOError Nothing NoSuchThing "" fileName Nothing Nothing

with :: ActionType -> SpecWith (Either SyncError (TestState, TestOutput)) -> Spec
with = before . pure . runIdentity . runTestResultT defaultBootstrappedConfig Nothing

withFileContents
  :: ActionType -> SpecWith (Text -> Either SyncError (TestState, TestOutput)) -> Spec
withFileContents actionType = before . pure $ \fileContents ->
  runIdentity $ runTestResultT defaultBootstrappedConfig (Just fileContents) actionType

given :: [Text] -> (b -> Expectation) -> (Text -> b) -> Expectation
given fileContents handler respond = handler . respond $ T.intercalate "\n" fileContents


e2eSpec :: Spec
e2eSpec = do
  describe "End to end specs" $ do


    withFileContents (SyncAction "test.org" (Just "The List")) $ do
      describe "Synchronization action" $ do
        it "should import the reminders from source to destination"
          $ given ["* org item"]
          $ \res -> length . reminders . fst <$> res `shouldBe` Right 1

        it "drop leading * symbols and spaces from reminder titles"
          $ given ["***  item"]
          $ \res -> fmap todoName . reminders . fst <$> res `shouldBe` Right ["item"]

        xit "flatten the hierarchy of org items"
          $ given ["* parent", "** child"]
          $ \res -> fmapDefault todoName . reminders . fst <$> res `shouldBe` Right
              ["parent", "child"]

        it "should produce info message when import starts" $ given ["* item"] $ \res ->
          let Right (_, TestOutput { infoLogs = infos }) = res
              Just foundMessage = L.find (T.isPrefixOf "Processing")
                $ fmap (E.decodeUtf8 . fromLogStr) infos
          in  foundMessage `shouldBe` "Processing test.org"


    with (SyncAction "non-existing.org" (Just "The List")) $ do
      describe "Synchronization of non-existing file" $ do

        it "shoud give an error - file is missing" $ \res ->
          reminders . fst <$> res `shouldBe` Left
            (FileReadError "non-existing.org" "does not exist (non-existing.org)")


    with (SyncAction "config.yaml" (Just "The List")) $ do
      describe "Synchronization of the file without .org extension" $ do

        xit "shoud show an error - wrong file extension" $ \res ->
          reminders . fst <$> res `shouldBe` Left
            (FileReadError "non-existing.org" "does not exist (non-existing.org)")

        xit "should write corresponding error to error log"
          $ \res -> safeHead . errorLogs . snd <$> res `shouldBe` (Right . Just $ "")


    with (SyncAction "empty.org" (Just "The List")) $ do
      describe "Synchronization of empty org file" $ do

        it "shoud return an error that the file is missing"
          $ \res -> reminders . fst <$> res `shouldBe` Left (NoItemsError "empty.org")

        xit "should write corresponding message to error log"
          $ \res -> safeHead . errorLogs . snd <$> res `shouldBe` (Right . Just $ "")
