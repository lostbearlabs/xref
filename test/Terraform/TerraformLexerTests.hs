module Terraform.TerraformLexerTests (terraformLexerTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Terraform.TerraformLexer

terraformLexerTests :: TestTree
terraformLexerTests =
  testGroup
    "TerraformLexer Tests"
    [ testCase "ids" $
        tok "foo a_b x2 *" @?= [TokId "foo", TokId "a_b", TokId "x2", TokId "*"],
      testCase "brackets" $
        tok "[ ] { } ( )" @?= [TokArrayStart, TokArrayEnd, TokBlockStart, TokBlockEnd, TokLeftParen, TokRightParen],
      testCase "equals" $
        tok "=" @?= [TokEquals],
      testCase "dot" $
        tok "." @?= [TokDot],
      testCase "dot2" $
        tok "x.y" @?= [TokId "x", TokDot, TokId "y"],
      testCase "numbers" $
        -- TODO: .8 is not handled correctly yet
        tok "123 4.56 7." @?= [TokNum "123", TokNum "4.56", TokNum "7."],
      testCase "booleans" $
        tok "true false" @?= [TokBool True, TokBool False],
      testCase "strings" $
        tok "\"fnord\"" @?= [TokStr "fnord"],
      testCase "heredocc" $
        tok "<<EOT\na\nb\nEOT" @?= [TokStr "a\nb\n"],
      testCase "comment" $
        tok "1 # 2 3 4\n" @?= [TokNum "1", TokSep]
    ]


tok :: String -> [TfToken]
tok = tokenizeTF ""

