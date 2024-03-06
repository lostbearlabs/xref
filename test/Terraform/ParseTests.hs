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
    ]





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
