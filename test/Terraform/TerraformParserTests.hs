module Terraform.TerraformParserTests (terraformParserTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Terraform.TerraformParser

terraformParserTests :: TestTree
terraformParserTests =
  testGroup
    "TerraformParser Tests"
    [ testCase "Variable" $
        actVariable @?= expVariable
    , testCase "Resource" $
        actResource @?= expResource
    , testCase "Module" $
        actModule @?= expModule
    , testCase "Config" $
        actConfig @?= expConfig
    , testCase "Data" $
        actData @?= expData
    , testCase "Output" $
        actOutput @?= expOutput
    , testCase "Provider" $
        actProvider @?= expProvider
    , testCase "Expressions" $
        actExpressions @?= expExpressions
    ]

-------------------------------------
-- Variable

actVariable :: [TfDeclaration]
actVariable = parseTF "stVariable" stVariable

expVariable :: [TfDeclaration]
expVariable = [TfVariable "var1" [ ("description", TStr "Var1 Description"), ("default", TNum "5678")] ]

stVariable :: String
stVariable = "variable \"var1\" {\
\  description = \"Var1 Description\"\
\  default     = 5678\
\}"

-------------------------------------
-- Provider

actProvider :: [TfDeclaration]
actProvider = parseTF "stProvider" stProvider

expProvider :: [TfDeclaration]
expProvider = [TfProvider "var1" [ ("description", TStr "Provider Description")] ]

stProvider :: String
stProvider = "provider \"var1\" {\
\  description = \"Provider Description\"\
\}"


-------------------------------------
-- Resource

actResource :: [TfDeclaration]
actResource = parseTF "stResource" stResource

expResource :: [TfDeclaration]
expResource = [TfResource "resource_type" "resource1" [("resource_tag", TStr "ResourceTag1")]]

stResource :: String
stResource = "\
\resource \"resource_type\" \"resource1\" {\
\  resource_tag          = \"ResourceTag1\"\
\}"

-------------------------------------
-- Module

actModule :: [TfDeclaration]
actModule = parseTF "stModule" stModule

expModule :: [TfDeclaration]
expModule = [TfModule "module_type" [("module_tag", TStr "ModuleTag1")]]

stModule :: String
stModule = "\
\module \"module_type\" {\
\  module_tag          = \"ModuleTag1\"\
\}"

-------------------------------------
-- Config

actConfig :: [TfDeclaration]
actConfig = parseTF "stConfig" stConfig

expConfig :: [TfDeclaration]
expConfig = [TfConfig [("foo", TStr "bar")]]

stConfig :: String
stConfig = "\
\terraform {\
\  foo          = \"bar\"\
\}"

-------------------------------------
-- Data

actData :: [TfDeclaration]
actData = parseTF "stData" stData

expData :: [TfDeclaration]
expData = [TfData "datatype" "datakey" [("foo", TStr "bar")]]

stData :: String
stData = "\
\data \"datatype\" \"datakey\" {\
\  foo          = \"bar\"\
\}"

-------------------------------------
-- Output

actOutput :: [TfDeclaration]
actOutput = parseTF "stOutput" stOutput

expOutput :: [TfDeclaration]
expOutput = [TfOutput "Output_type" [("Output_tag", TStr "OutputTag1")]]

stOutput :: String
stOutput = "\
\output \"Output_type\" {\
\  Output_tag          = \"OutputTag1\"\
\}"


-------------------------------------
-- Expressions
-- (a simple config block as tested previously, but now exercising the different possible RVals)

actExpressions :: [TfDeclaration]
actExpressions = parseTF "stExpressions" stExpressions

expExpressions :: [TfDeclaration]
expExpressions = [TfConfig 
    [ ("foo", TStr "bar")
    , ("foo2", TStr "bar2")
    , ("a", TNum "123")
    , ("b", TBool True)
    , ("c", TArray [ TStr "x", TStr "y"])
    , ("d", TMap [ ("p", TStr "p1"), ("q", TStr "q1") ])
    ]]

stExpressions :: String
stExpressions = "\
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
\}"
