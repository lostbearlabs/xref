module Main (main) where

import XData

main :: IO ()
main = do
  let filePath = "xref.json"
  db <- readDatabaseFromFile filePath
  case db of
    Just dbb -> do
      putStrLn ("Read " ++ show (length (defs dbb)) ++ " symbols from " ++ filePath)
    Nothing -> do
      putStrLn ("Unable to read " ++ filePath)
