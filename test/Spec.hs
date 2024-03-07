import DiffTests
import Test.Tasty
import XDataTests
import Terraform.ParseTests
import Terraform.LexerTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [xDataTests, diffTests, 
                parseTests, lexerTests]
