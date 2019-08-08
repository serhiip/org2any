{-# LANGUAGE OverloadedStrings #-}

module AppleScriptSpec where

import           Test.Hspec
import           AppleScript.Internal
import           Types

remindersSpec :: Spec
remindersSpec = describe "Reminders.App MacOS command generator" $ do

  it "shoud list all reminders, remove unwanted parts, and convert dates" $ do
    listAllScript
      `shouldBe` "var r = Application('Reminders'); \n\
                \var rems = r.defaultList.reminders(); \n\
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
    (createManyScript . remindersFromList $ [Reminder "test" "test1" "test2" Nothing])
      `shouldBe` "app = Application(\"Reminders\"); \n\
                   \app.defaultList.reminders.push(app.Reminder(\
                     \{\"name\":\"test |test1\",\"body\":\"test2\",\"completed\":false}\
                   \));"

  it "should delete an reminders by ID" $ do
    (deleteManyScript . remindersFromList $ [Reminder "test" "test1" "test2" Nothing])
      `shouldBe` "app = Application('Reminders'); \n\
                   \items = app.defaultList.reminders(); \n\
                   \const deletions = \
                     \{\"test1\":{\"name\":\"test |test1\",\"body\":\"test2\",\"completed\":false}}; \n\
                     \for (var item of items) { \n\
                       \const [name, id] = item.name().split('|', 2); \n\
                       \if (id in deletions) { \n\
                         \app.delete(item); \n\
                       \} \n\
                     \}"

  it "should update reminder attribute by attribute if ID matches" $ do
    (updateManyScript . remindersFromList $ [Reminder "test" "test1" "test2" Nothing])
      `shouldBe` "app = Application('Reminders'); \n\
                   \items = app.defaultList.reminders(); \n\
                   \updates = \
                     \{\"test1\":{\"name\":\"test |test1\",\"body\":\"test2\",\"completed\":false}}; \n\
                   \for (const item of items) { \nconst [name, id] = item.name().split('|', 2); \n\
                     \if (id in updates) { \n\
                       \const to = updates[id]; \n\
                       \for (const attr_name in to) { \n\
                         \const upd = to[attr_name]; \n\
                         \if (item[attr_name]() != upd) { \n\
                           \item[attr_name] = upd \n\
                         \} \n\
                       \} \n\
                     \} \n\
                   \}"
