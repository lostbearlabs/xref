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
    , testCase "Config" $
        actConfig @?= expConfig
    , testCase "Variable" $
        actVariable @?= expVariable
    ]




-------------------------------------
-- Variable


actVariable :: E [TDeclaration]
actVariable = parseInput stVariable

expVariable :: E [TDeclaration]
expVariable = Ok [TVariable "var1" [ ("description", TStr "Var1 Description"), ("default", TNum 5678)] ]

stVariable :: String
stVariable = "variable \"var1\" {\
\  description = \"Var1 Description\"\
\  default     = 5678\
\}"


-------------------------------------
-- Config

actConfig :: E [TDeclaration]
actConfig = parseInput stConfig

expConfig :: E [TDeclaration]
expConfig = Ok [TConfig [("foo", TStr "bar")]]

stConfig :: String
stConfig = "\
\terraform {\
\  foo          = \"bar\"\
\}"
