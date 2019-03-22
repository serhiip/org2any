module Parser
  ( titles
  , doc
  , runParser
  , reminders
  ) where

import           Control.Monad        (mzero)
import qualified Data.Attoparsec.Text as A
import           Data.Bifunctor       (bimap)
import           Data.HashMap.Strict  (lookupDefault)
import qualified Data.OrgMode.Parse   as O
import qualified Data.OrgMode.Types   as O
import qualified Data.Text            as T
import           Types                (Reminder (..), Reminders)

newtype Org = Org
  { doc :: O.Document
  } deriving (Show)

runParser :: T.Text -> Either String Org
runParser = bimap verboseError Org . A.parseOnly parser
  where
    parser = O.parseDocument mzero
    verboseError = flip (++) "Error parsing Org File: "

newtype Title = Title {text :: T.Text} deriving Show

titles :: O.Headline -> [(T.Text, T.Text)]
titles item = (O.title item, rid item) : concat (titles <$> O.subHeadlines item)
  where rid = lookupId . O.unProperties . O.sectionProperties . O.section
        lookupId = lookupDefault (T.pack "noid") (T.pack "CUSTOM_ID")

reminders :: Org -> Reminders
reminders =  fmap (uncurry Reminder) . concat . (titles <$>) . O.documentHeadlines . doc
