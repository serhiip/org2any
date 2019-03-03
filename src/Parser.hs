module Parser
  ( titles
  , doc
  , runParser
  , reminders
  ) where

import           Command              (Reminder (..), Reminders)
import           Control.Monad        (mzero)
import qualified Data.Attoparsec.Text as A
import           Data.Bifunctor       (bimap)
import qualified Data.OrgMode.Parse   as O
import qualified Data.OrgMode.Types   as O
import qualified Data.Text            as T

newtype Org = Org
  { doc :: O.Document
  } deriving (Show)

runParser :: T.Text -> Either String Org
runParser = bimap verboseError Org . A.parseOnly parser
  where
    parser = O.parseDocument mzero
    verboseError = flip (++) "Error parsing Org File: "

newtype Title = Title {text :: T.Text} deriving Show

titles :: O.Headline -> [T.Text]
titles item = O.title item : concat (titles <$> O.subHeadlines item)

reminders :: Org -> Reminders
reminders =  fmap Reminder . concat . (titles <$>) . O.documentHeadlines . doc
