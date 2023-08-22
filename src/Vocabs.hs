module Vocabs
    ( wordsByFrequency
    , frequentWordsReport
    , allWords
    , allWordsReport
    , wordsCount
    , wordsCountReport
    , wordsCountReport'
    , processTextFile'
    ) where

import Fmt
    ( fmt
    , nameF
    , unlinesF
    , build
    , blockListF'
    , (+|)
    , (|+)
    )
import Data.List (sortBy)
import Vocabulary (Vocabulary, extractVocab)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Ord (comparing, Down (Down))
import Control.Monad (when)

allWords :: Vocabulary -> [Text]
allWords = map fst

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocab = (length vocab, sum $ map snd vocab)

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

allWordsReport :: Vocabulary -> Text
allWordsReport vocab =
    fmt $ nameF (build "All words") $ unlinesF (allWords vocab)

frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocab num =
    fmt $ nameF (build "Frequent words")
        $ blockListF' (T.pack "") fmtEntry reportData
  where
    reportData = take num $ wordsByFrequency vocab
    fmtEntry (t, n) = build "" +|t|+ build ": " +|n|+ build ""


wordsCountReport :: Vocabulary -> Text
wordsCountReport vocab = T.unlines [part1, part2]
  where
    (total, unique) = wordsCount vocab
    part1 = T.append (T.pack "Total number of words: ")
                     (T.pack $ show total)
    part2 = T.append (T.pack "Number of unique words: ")
                     (T.pack $ show unique)

wordsCountReport' :: Vocabulary -> Text
wordsCountReport' vocab = fmt $
    build "Total number of words: " +|total|+
    build "\nNumber of unique words: " +|unique|+ build "\n"
  where
    (total, unique) = wordsCount vocab

processTextFile' :: FilePath -> Bool -> Int -> IO ()
processTextFile' fname withAllWords n = do
    text <- TIO.readFile fname
    let vocab = extractVocab text
    when withAllWords $ TIO.putStrLn $ allWordsReport vocab
    TIO.putStrLn $ wordsCountReport vocab
    TIO.putStrLn $ frequentWordsReport vocab n
