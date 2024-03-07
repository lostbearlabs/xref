module Terraform.LexerTests (lexerTests) where
import Test.Tasty
import Test.Tasty.HUnit
import Terraform.Lexer
import Terraform.Parse(lexInput)

lexerTests :: TestTree
lexerTests =
  testGroup
    "Lexer Tests"
    [ testCase "ws" $
        lexInput "   " @?= []
    , testCase "keywords" $
        lexInput "terraform variable provider resource data" @?= 
            [TkTerraform, TkVariable, TkProvider, TkResource, TkData]
    , testCase "int" $
        lexInput "2" @?= [TkInt 2]
    , testCase "bool" $
        lexInput "True False true false" @?= [TkBool True, TkBool False, TkBool True, TkBool False]
    , testCase "ops" $
        lexInput "+ * - / =" @?= [TkPlus, TkMult, TkMinus, TkDiv, TkEquals]
    , testCase "brackets" $
        lexInput "( ) [ ] { }" @?= [TkLParen, TkRParen, TkArrayStart, TkArrayEnd, TkBlockStart, TkBlockEnd]
    , testCase "punctuation" $
        lexInput ". ? : ," @?= [TkDot, TkQuestion, TkColon, TkComma]
    , testCase "quotedString" $
        lexInput "\"a b c\"" @?= [TkStr "a b c"]
    , testCase "identifiers" $
        lexInput "a a2 a_2" @?= [TkId "a", TkId "a2", TkId "a_2"]
    , testCase "heredoc" $
        lexInput "<<EOT\nfoo\nbar\nEOT" @?= [TkStr "foo\nbar"]
    ]

