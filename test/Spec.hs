import DiffTests
import Test.Tasty
import XDataTests
import Terraform.TerraformParserTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [xDataTests, diffTests, terraformParserTests]
