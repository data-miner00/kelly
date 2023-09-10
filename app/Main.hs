module Main (main) where

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           System.Environment
import           TicTacToe          (Piece (Open, Player), play)
import           Vocab
import           Vocabs
import           Vocabulary

main :: IO ()
main = play board 'X'
  where
    board = [Open 1, Open 2, Open 3, Open 4, Open 5, Open 6, Open 7, Open 8, Open 9]

unusedMain :: IO ()
unusedMain = do
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
