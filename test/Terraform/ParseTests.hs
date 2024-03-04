module Terraform.ParseTests (parseTests) where
import Test.Tasty
import Test.Tasty.HUnit
import Terraform.ParserDependencies
import Terraform.Parse(parseInput)

parseTests :: TestTree
parseTests =
  testGroup
    "Parser Tests"
    [ 
      testCase "error" $ 
        case parseInput "22 22" of
          Failed _ -> return ()
          _ -> assertFailure "Expected failure, but parsing succeeded"
    , testCase "int" $
        (parseInput "2") @?= (Ok 2)
    ]



