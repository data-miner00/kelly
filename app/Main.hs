module Main (main) where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import Vocab

main :: IO ()
main = do
    [filename] <- getArgs
    text <- readFile filename
    TIO.putStrLn $ T.unwords $ extract $ T.pack text
