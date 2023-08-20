module Vocab
    ( extract
    ) where

import Data.Char
import Data.List (group, sort)
import qualified Data.Text as T

extract :: T.Text -> [T.Text]
extract text = do
    let ws = map head $ group $ sort $ map T.toCaseFold $ filter (not . T.null)
             $ map (T.dropAround $ not . isLetter) $ T.words text
    ws
