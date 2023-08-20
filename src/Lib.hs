module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    putStrLn "What is your name?"
    line <- getLine
    putStrLn ("someFunc: " ++ line)
    contents <- readFile "stack.yaml"
    putStrLn contents
