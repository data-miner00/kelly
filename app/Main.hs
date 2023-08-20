module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import Vocab
import Vocabulary

main :: IO ()
main = do
    [filename] <- getArgs
    text <- readFile filename
    TIO.putStrLn $ T.unwords $ extract $ T.pack text

    args <- getArgs
    case args of
        [fname] -> processTextFile fname
        _ -> putStrLn "Usage: vocab-builder filename"
