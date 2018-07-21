module Parser where

import           Control.Monad        (mzero)
import qualified Data.Attoparsec.Text as A
import           Data.Bifunctor       (bimap)
import qualified Data.OrgMode.Parse   as O
import qualified Data.OrgMode.Types   as O
import           Data.Text

data Org = Org
  { doc :: O.Document
  } deriving (Show)

runParser :: Text -> Either String Org
runParser = (bimap verboseError Org) . (A.parseOnly parser)
  where
    parser = O.parseDocument mzero
    verboseError = (flip (++)) "Error parsing Org File: "
