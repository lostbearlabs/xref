module XDataTests (xDataTests) where

import Test.Tasty
import Test.Tasty.HUnit
import XData

xDataTests :: TestTree
xDataTests =
  testGroup
    "XData Tests"
    [ testCase "Example Test" $
        aDef @?= aDef
    ]

aDef :: Def
aDef = Def {symbol = "Sym", file = "File.txt"}
