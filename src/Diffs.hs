module Diffs (module Diffs) where

import qualified Data.Set as Set
import XData


unmatchedRefs :: Database -> [Ref]
unmatchedRefs db = filter (\r -> Set.notMember (targetSymbol r) known) (refs db)
  where
    known = Set.fromList (map symbol (defs db))

unusedDefs :: Database -> [Def]
unusedDefs db = filter (\d -> Set.notMember (symbol d) known) (defs db)
  where
    known = Set.fromList (map targetSymbol (refs db))
