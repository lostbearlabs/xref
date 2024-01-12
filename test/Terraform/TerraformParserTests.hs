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
