module Main (main) where

import XData

main :: IO ()
main = do
  let filePath = "xref.json"
  let db = sampleDb
  putStrLn ("Writing " ++ (show (length (defs db))) ++ " symbols to " ++ filePath ++ " ...")
  writeDatabaseToFile filePath db
  putStrLn "... done"
