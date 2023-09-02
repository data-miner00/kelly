module Maybe where

import qualified Data.Maybe as Maybe'

a :: Bool
a = Maybe'.isJust $ Just 5

b :: Bool
b = Maybe'.isNothing Nothing

unwrapped :: Int
unwrapped = Maybe'.fromJust $ Just 5

safeUnwrapped :: Int
safeUnwrapped = Maybe'.fromMaybe 0 $ Just 3
