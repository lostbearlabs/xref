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
    , testCase "Provider" $
        actProvider @?= expProvider
    , testCase "Resource" $
        actResource @?= expResource
    , testCase "Module" $
        actModule @?= expModule
    , testCase "Data" $
        actData @?= expData
    , testCase "Output" $
        actOutput @?= expOutput
    , testCase "Expressions" $
        actExpressions @?= expExpressions
    , testCase "ValueTypes" $
        actValueTypes @?= expValueTypes
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
-- Provider

actProvider :: E [TDeclaration]
actProvider = parseInput stProvider

expProvider :: E [TDeclaration]
expProvider = Ok [TProvider "var1" [ ("description", TStr "Provider Description")] ]

stProvider :: String
stProvider = "provider \"var1\" {\
\  description = \"Provider Description\"\
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

-------------------------------------
-- Resource

actResource :: E [TDeclaration]
actResource = parseInput stResource

expResource :: E [TDeclaration]
expResource = Ok [TResource "resource_type" "resource1" [("resource_tag", TStr "ResourceTag1")]]

stResource :: String
stResource = "\
\resource \"resource_type\" \"resource1\" {\
\  resource_tag          = \"ResourceTag1\"\
\}"

-------------------------------------
-- Module

actModule :: E [TDeclaration]
actModule = parseInput stModule

expModule :: E [TDeclaration]
expModule = Ok [TModule "module_type" [("module_tag", TStr "ModuleTag1")]]

stModule :: String
stModule = "\
\module \"module_type\" {\
\  module_tag          = \"ModuleTag1\"\
\}"

-------------------------------------
-- Data

actData :: E [TDeclaration]
actData = parseInput stData

expData :: E [TDeclaration]
expData = Ok [TData "datatype" "datakey" [("foo", TStr "bar")]]

stData :: String
stData = "\
\data \"datatype\" \"datakey\" {\
\  foo          = \"bar\"\
\}"

-------------------------------------
-- Output

actOutput :: E[TDeclaration]
actOutput = parseInput stOutput

expOutput :: E [TDeclaration]
expOutput = Ok [TOutput "Output_type" [("Output_tag", TStr "OutputTag1")]]

stOutput :: String
stOutput = "\
\output \"Output_type\" {\
\  Output_tag          = \"OutputTag1\"\
\}"


-------------------------------------
-- Expressions
-- (a simple config block as tested previously, but now exercising the different expressions that can be used in RVals)

actExpressions :: E [TDeclaration]
actExpressions = parseInput stExpressions

expExpressions :: E [TDeclaration]
expExpressions = Ok [TConfig
    [ ("a", TRId "x")
    , ("b", TRef "x" (TRId "y"))
    , ("c", TNum 1)
    , ("d", TBool False)
    , ("e", TFunc "func" [TNum 1, TNum 2])

    -- , ("foo2", TStr "bar2")
    -- , ("a", TNum "123")
    -- , ("b", TBool True)
    -- , ("c", TArray [ TStr "x", TStr "y"])
    -- , ("d", TMap [ ("p", TStr "p1"), ("q", TStr "q1") ])
    -- , ("e", TMap [ ("r", TStr "r1") ])
    ]]

stExpressions :: String
stExpressions = "\
\terraform {\
\  a          = x\
\  b          = x.y\
\  c          = 1\
\  d          = false\
\  e          = func( 1, 2 )\
\}"

-------------------------------------
-- ValueTypes
-- (a simple config block as tested previously, but now exercising the different shapes for RVals)

actValueTypes :: E [TDeclaration]
actValueTypes = parseInput stValueTypes

expValueTypes :: E [TDeclaration]
expValueTypes = Ok [TConfig
    [ ("foo", TStr "bar")
    , ("foo2", TStr "bar2")
    , ("a", TNum 123)
    , ("b", TBool True)
    , ("c", TArray [ TStr "x", TStr "y"])
    , ("d", TMap [ ("p", TStr "p1"), ("q", TStr "q1") ])
    , ("e", TMap [ ("r", TStr "r1") ])
    ]]

stValueTypes :: String
stValueTypes = "\
\terraform {\
\  foo          = \"bar\"\
\  foo2         = \"bar2\"\
\  a            = 123\
\  b            = true\
\  c            = [ \"x\", \"y\" ]\
\  d            = {\
\                    p = \"p1\",\
\                    q = \"q1\"\
\                 }\
\  e            = {\
\                    r = \"r1\",\
\                 }\
\}"

