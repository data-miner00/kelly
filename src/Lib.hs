module Lib
    ( someFunc
    ) where

import System.IO (readFile)

someFunc :: IO ()
someFunc = do
    putStrLn "What is your name?"
    line <- getLine
    putStrLn ("someFunc: " ++ line)
    contents <- readFile "stack.yaml"
    putStrLn contents

