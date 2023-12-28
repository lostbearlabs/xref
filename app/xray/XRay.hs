module Main (main) where

import XData

main :: IO ()
main = do 
    putStrLn "XRay!"
    putStrLn (show (Def "symbol" "myfile.txt"))