module Main (main) where

import XData

main :: IO ()
main = do 
    putStrLn "XRef!"
    putStrLn (show (Def "symbol" "myfile.txt"))