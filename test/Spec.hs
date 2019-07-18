import           LibSpec
import           AppleScriptSpec

import           Test.Hspec

main :: IO ()
main = hspec $ do
  spec
  remindersSpec
