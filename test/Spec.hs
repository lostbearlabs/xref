import DiffTests
import Test.Tasty
import XDataTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [xDataTests, diffTests]
