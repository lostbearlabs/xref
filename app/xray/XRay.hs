module Main (main) where

import XData

main :: IO ()
main = do
  let filePath = "xref.json"
  let db = sampleDb
  putStrLn ("Writing " ++ (show (length (defs db))) ++ " symbols to " ++ filePath ++ " ...")
  writeDatabaseToFile filePath db
  putStrLn "... done"

sampleDb :: Database
sampleDb =
  Database
    [ Def "a" "fileA",
      Def "b" "fileB"
    ]
    [ Ref "a" "b"
    ]
