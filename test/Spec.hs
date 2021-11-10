import AppleScriptSpec ( remindersSpec )
import E2ESpec ( e2eSpec )
import CommandSpec ( commandSpec )
import Test.Hspec ( hspec )
import Universum


main :: IO ()
main = hspec $ do
  e2eSpec
  commandSpec
  remindersSpec
