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

items :: Text -- ^ Contents of org file
  -> Maybe Reminders
items fileContents = convertSections Nothing . orgDoc <$> parsedFile
 where
  parsedFile = org fileContents
  convertSections parent = concatMap toReminder . docSections
   where
    toReminder :: Section -> Reminders
    toReminder sec =
      let rid      = fromJust . lookup "ID" . sectionProps $ sec
          title    = sconcat . map prettyWords . sectionHeading $ sec
          doc      = sectionDoc sec
          body     = unlines . map block . docBlocks $ doc
          status   = todo <$> sectionTodo sec
          children = convertSections (Just rid) doc
      in  Reminder title rid (Just body) status rid parent : children

    todo TODO = Todo
    todo DONE = Done

    block :: Block -> Text
    block (Quote   txt ) = txt
    block (Example txt ) = txt
    block (Code _ _    ) = ""
    block (List      _ ) = ""
    block (Table     _ ) = ""
    block (Paragraph ne) = sconcat $ map prettyWords ne
