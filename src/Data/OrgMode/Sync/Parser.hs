{-|
Module      : Data.OrgMode.Sync.Parser
Description : Org file parsing utilities
License     : GPL-3
Maintainer  : Serhii <serhii@proximala.bz>
Stability   : experimental

Org file parsing utilities via
 <https://github.com/ixmatus/orgmode-parse orgmode-parse>.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.OrgMode.Sync.Parser
  ( titles
  , runParser
  , reminders
  )
where

import           Universum

import qualified Data.Attoparsec.Text          as A
import           Data.Bifunctor                 ( first )
import           Data.HashMap.Strict.InsOrd     ( lookupDefault )
import qualified Data.OrgMode.Parse            as O
import qualified Data.OrgMode.Types            as O
import qualified Data.Text                     as T
import           Data.OrgMode.Sync.Types        ( Reminder(..)
                                                , Reminders
                                                , TodoStatus(..)
                                                , SyncError(..)
                                                )
import           Data.Monoid                    ( mconcat )

-- | Execute a parser against given string
runParser :: T.Text -> Either SyncError O.Document
runParser = first (SysCallError . fromString) . A.parseOnly O.parseDocument

-- | Convert org document headlines to internal representation
titles :: (Applicative m, Monoid (m Reminder)) => O.Headline -> m Reminder
titles h@O.Headline {..} = pure (Reminder title (rid section) (body h) status)
  <> mconcat (titles <$> O.subHeadlines h)
 where
  rid      = lookupId . O.unProperties . O.sectionProperties
  lookupId = lookupDefault (T.pack "noid") (T.pack "ID")
  headl    = O.sectionContents . O.section

  body (headl -> []) = Just T.empty
  body (headl -> [O.Paragraph [O.Plain txt]]) = Just txt
  body _ = Nothing

  status = O.stateKeyword h >>= mkStatus . T.unpack . O.unStateKeyword

  mkStatus "DONE" = Just Done
  mkStatus "TODO" = Just Todo
  mkStatus _      = Nothing

-- | Extract a list of reminders from parsed org document
reminders :: O.Document -> Reminders
reminders = concatMap titles . O.documentHeadlines
