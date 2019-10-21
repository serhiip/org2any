{-# LANGUAGE OverloadedStrings #-}

module AppleScriptSpec where

import           Data.OrgMode.Sync.AppleScript.Internal
import           Test.Hspec
import           Data.OrgMode.Sync.Types
import           Universum

remindersSpec :: Spec
remindersSpec = describe "Reminders.App MacOS command generator" $ do

  it "shoud list all reminders, remove unwanted parts, and convert dates" $ do
    listAllScript "TODOs"
      `shouldBe` "app = Application('Reminders'); \n\
                \list = app.lists().filter(_ => _.id() == 'TODOs')[0]; \n\
                \var rems = list.reminders(); \n\
                \const res = rems.map(reminder => { \n\
	            \const props = reminder.properties(); \n\
	            \props['container'] = null; \n\
	            \for (var property in props) \n\
                        \if (props.hasOwnProperty(property)) \n\
		            \if (props[property] && props[property].toISOString) \n\
			        \props[property] = props[property].toISOString() \n\
	            \return props \n\
                \}) \n\
                \JSON.stringify(res)"

  it "should create the reminders by pushing them to existing list of reminders" $ do
    createManyScript "TODOs" (to <$> [Reminder "test" "test1" (Just "test2") Nothing "test1"])
      `shouldBe` "app = Application(\"Reminders\"); \napp.defaultList.reminders.push(app.Reminder({\"id\":\"test1\",\"body\":\"test2\",\"completed\":false,\"name\":\"test |test1\",\"priority\":0,\"dueDate\":null,\"modificationDate\":null,\"creationDate\":null,\"completionDate\":null,\"remindMeDate\":null}));"

  it "should delete an reminders by ID" $ do
    deleteManyScript "TODOs" (to <$> [Reminder "test" "test1" (Just "test2") Nothing "test1"])
      `shouldBe` "app = Application('Reminders'); \nlist = app.lists().filter(_ => _.id() == 'TODOs')[0];\nitems = list.reminders(); \nconst deletions = {\"test1\":{\"id\":\"test1\",\"body\":\"test2\",\"completed\":false,\"name\":\"test |test1\",\"priority\":0,\"dueDate\":null,\"modificationDate\":null,\"creationDate\":null,\"completionDate\":null,\"remindMeDate\":null}}; \nfor (var item of items) { \nconst [name, id] = item.name().split('|', 2); \nif (id in deletions) { \napp.delete(item); \n} \n}"

  it "should update reminder attribute by attribute if ID matches" $ do
    updateManyScript "TODOs" (to <$> [Reminder "test" "test1" (Just "test2") Nothing "test1"])
      `shouldBe` "app = Application('Reminders'); \nlist = app.lists().filter(_ => _.id() == 'TODOs')[0];\nitems = list.reminders(); \nupdates = {\"test1\":{\"id\":\"test1\",\"body\":\"test2\",\"completed\":false,\"name\":\"test |test1\",\"priority\":0,\"dueDate\":null,\"modificationDate\":null,\"creationDate\":null,\"completionDate\":null,\"remindMeDate\":null}}; \nfor (const item of items) { \nconst [name, id] = item.name().split('|', 2); \nif (id in updates) { \nconst to = updates[id]; \nfor (const attr_name in to) { \nconst upd = to[attr_name]; \nif (upd && attr_name != 'id' && item[attr_name]() != upd) { \nitem[attr_name] = upd \n} \n} \n} \n}"

  it "should allow to list all of the ixisting Reminders lists" $ do
    listListsScript
      `shouldBe` "app = Application('Reminders'); \n\
  \const res = app.lists().map(reminder => { \n\
    \const props = reminder.properties(); \n\
    \props['container'] = null; \n\
    \for (var property in props) \n\
      \if (props.hasOwnProperty(property)) \n\
        \if (props[property] && props[property].toISOString) \n\
          \props[property] = props[property].toISOString(); \n\
    \return props; \n\
  \}); \n\
  \JSON.stringify(res);"
