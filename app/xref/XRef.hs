module Main (main) where

import Diffs
import XData

main :: IO ()
main = do
  let filePath = "xref.json"
  db <- readDatabaseFromFile filePath
  case db of
    Just dbb -> do
      putStrLn ("Read " <> show (length (defs dbb)) <> " symbols from " <> filePath)
      printDiffs dbb
    Nothing -> do
      putStrLn ("Unable to read " <> filePath)

printDiffs :: Database -> IO ()
printDiffs db = do
  putStrLn "Unused defs:"
  printListElements (unusedDefs db)
  putStrLn "Unmatched refs:"
  printListElements (unmatchedRefs db)

printListElements :: (Show a) => [a] -> IO ()
printListElements xs = mapM_ putStrLn (map (\x -> "   " <> show x) xs)
