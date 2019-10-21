import           AppleScriptSpec
import           CommandSpec
import           Test.Hspec
import           Universum


main :: IO ()
main = hspec $ do
  commandSpec
  remindersSpec
