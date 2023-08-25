module Main (main) where

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           System.Environment
import           Vocab
import           Vocabs
import           Vocabulary

main :: IO ()
main = do
    {-
    [filename] <- getArgs
    text <- readFile filename
    TIO.putStrLn $ T.unwords $ extract $ T.pack text
    -}

    args <- getArgs
    case args of
        ["-a", fname, num] ->
            processTextFile' fname True (read num)
        [fname, num] ->
            processTextFile' fname False (read num)
        [fname] -> processTextFile fname
        _ -> putStrLn "Usage: kelly-exe [-a] filename [freq_words_num]"
