import DiffTests
import Test.Tasty
import XDataTests
import Terraform.TerraformParserTests
import Terraform.TerraformLexerTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [xDataTests, diffTests, terraformLexerTests, terraformParserTests]
