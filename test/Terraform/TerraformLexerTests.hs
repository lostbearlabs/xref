module Terraform.TerraformLexerTests (terraformLexerTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Terraform.TerraformLexer

terraformLexerTests :: TestTree
terraformLexerTests =
  testGroup
    "TerraformLexer Tests"
    [ testCase "Dummy" $
        "foo" @?= "foo"
    ]
