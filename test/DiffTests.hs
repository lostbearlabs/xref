module DiffTests (module DiffTests) where

import Diffs
import Test.Tasty
import Test.Tasty.HUnit
import XData

diffTests :: TestTree
diffTests =
  testGroup
    "Diff Tests"
    [ testCase "expected unmatched refs" $
        (unmatchedRefs sampleDb) @?= [Ref "d" Nothing "fileY"],
      testCase "expected unused defs" $
        (unusedDefs sampleDb) @?= [Def "b" "fileB"]
    ]

aDef :: Def
aDef = Def {symbol = "Sym", file = "File.txt"}
