import           AppleScriptSpec
import           LibSpec
import           Test.Hspec
import           Universum


main :: IO ()
main = hspec $ do
  spec
  remindersSpec
