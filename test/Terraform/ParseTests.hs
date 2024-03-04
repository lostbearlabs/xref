module Terraform.ParseTests (parseTests) where
import Test.Tasty
import Test.Tasty.HUnit
import Terraform.Parse(parseInput)

parseTests :: TestTree
parseTests =
  testGroup
    "Parser Tests"
    [  testCase "int" $
        parseInput "2" @?= Right 2
    ]


