import           AppleScriptSpec
import           E2ESpec
import           CommandSpec
import           Test.Hspec
import           Universum


main :: IO ()
main = hspec $ do
  e2eSpec
  commandSpec
  remindersSpec
