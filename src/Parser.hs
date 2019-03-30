module Parser
  ( titles
  , doc
  , runParser
  , reminders
  ) where

import qualified Data.Attoparsec.Text       as A
import           Data.Bifunctor             (bimap)
import           Data.HashMap.Strict.InsOrd (lookupDefault)
import qualified Data.OrgMode.Parse         as O
import qualified Data.OrgMode.Types         as O
import           Data.Set                   (fromList)
import qualified Data.Text                  as T
import           Debug.Trace                (traceShowId)
import           Types                      (Reminder (..), Reminders,
                                             TodoStatus (..))

newtype Org = Org
  { doc :: O.Document
  } deriving (Show)

runParser :: T.Text -> Either String Org
runParser = bimap verboseError Org . A.parseOnly parser
  where
    parser = O.parseDocument
    verboseError = flip (++) "Error parsing Org File: "

newtype Title = Title {text :: T.Text} deriving Show

-- | Get a list of Reminders from parsed document
--   TODO: handle body printing properly
titles :: O.Headline -> [Reminder]
titles item =
  Reminder (O.title item) (rid item) (T.pack . show $ lookupBody item) status :
  concat (titles <$> O.subHeadlines item)
  where
    rid = lookupId . O.unProperties . O.sectionProperties . O.section
    lookupId = lookupDefault (T.pack "noid") (T.pack "CUSTOM_ID")
    lookupBody = O.sectionContents . O.section
    status = O.stateKeyword item >>= mkStatus . T.unpack . O.unStateKeyword
    mkStatus st =
      case st of
        "DONE" -> Just Done
        "TODO" -> Just Todo
        _      -> Nothing

reminders :: Org -> Reminders
reminders = fromList . concatMap titles . O.documentHeadlines . doc
