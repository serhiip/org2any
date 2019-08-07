{-# LANGUAGE NoImplicitPrelude #-}

module Parser
  ( titles
  , doc
  , runParser
  , reminders
  )
where

import           Universum

import qualified Data.Attoparsec.Text          as A
import           Data.Bifunctor                 ( bimap )
import           Data.HashMap.Strict.InsOrd     ( lookupDefault )
import qualified Data.OrgMode.Parse            as O
import qualified Data.OrgMode.Types            as O
import           Data.Set                       ( fromList )
import qualified Data.Text                     as T
import           Types                          ( Reminder(..)
                                                , Reminders
                                                , TodoStatus(..)
                                                )
import           Data.Monoid                    ( mconcat )

newtype Org = Org
  { doc :: O.Document
  } deriving (Show)

runParser :: T.Text -> Either String Org
runParser = bimap verboseError Org . A.parseOnly parser
 where
  parser       = O.parseDocument
  verboseError = flip (++) "Error parsing Org File: "

newtype Title = Title {text :: T.Text} deriving Show

titles :: (Applicative m, Monoid (m Reminder)) => O.Headline -> m Reminder
titles item = pure (Reminder (O.title item) (rid item) (lookupBody item) status)
  <> mconcat (titles <$> O.subHeadlines item)
 where
  rid        = lookupId . O.unProperties . O.sectionProperties . O.section
  lookupId   = lookupDefault (T.pack "noid") (T.pack "CUSTOM_ID")
  lookupBody headline = case (O.sectionContents . O.section) headline of
    [] -> T.empty
    [O.Paragraph [O.Plain txt]] -> txt
    _ -> T.pack "Org item body type is not supported"

  status     = O.stateKeyword item >>= mkStatus . T.unpack . O.unStateKeyword
  mkStatus st = case st of
    "DONE" -> Just Done
    "TODO" -> Just Todo
    _      -> Nothing

reminders :: Org -> Reminders
reminders = fromList . concatMap titles . O.documentHeadlines . doc
