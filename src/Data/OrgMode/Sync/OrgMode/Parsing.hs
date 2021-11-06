{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Sync.OrgMode.Parsing
  ( items
  ) where

import           Data.Map.Strict                ( lookup )
import           Data.Maybe                     ( fromJust )
import           Data.Org                       ( Block
                                                  ( Code
                                                  , Example
                                                  , List
                                                  , Paragraph
                                                  , Quote
                                                  , Table
                                                  )
                                                , ListItems(ListItems)
                                                , OrgDoc(docBlocks, docSections)
                                                , OrgFile(orgDoc)
                                                , Section
                                                  ( sectionDoc
                                                  , sectionHeading
                                                  , sectionProps
                                                  , sectionTodo
                                                  )
                                                , Todo(DONE, TODO)
                                                , org
                                                , prettyWords
                                                )
import           Data.OrgMode.Sync.Types        ( Reminder(Reminder)
                                                , Reminders
                                                , TodoStatus(..)
                                                )
import           Universum

items :: Text -> Maybe Reminders
items fileContents = convertSections <$> parsedFile
 where
  parsedFile      = org fileContents
  convertSections = map toReminder . docSections . orgDoc

  toReminder :: Section -> Reminder
  toReminder sec =
    let rid    = fromJust . lookup "ID" . sectionProps $ sec
        title  = sconcat . map prettyWords . sectionHeading $ sec
        body   = unlines . map block . docBlocks . sectionDoc $ sec
        status = todo <$> sectionTodo sec
    in  Reminder title rid (Just body) status rid

  todo TODO = Todo
  todo DONE = Done

  block :: Block -> Text
  block (Quote   txt            ) = txt
  block (Example txt            ) = txt
  block (Code _ _               ) = ""
  block (List      (ListItems _)) = ""
  block (Table     _            ) = ""
  block (Paragraph ne           ) = sconcat $ map prettyWords ne
