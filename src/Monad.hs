module Monad where

import           Data.Maybe

maybeadd :: Num b => Maybe b -> b -> Maybe b
maybeadd mx y = mx >>= (\x -> Just $ x + y)

maybeadd' :: Num b => Maybe b -> Maybe b -> Maybe b
maybeadd' mx my =
    mx >>= (\x -> my >>= (\y -> Just $ x + y))

maybeadd'' :: (Monad m, Num b) => m b -> m b -> m b
maybeadd'' mx my =
    mx >>= (\x -> my >>= (\y -> return $ x + y))

maybeadd''' :: (Monad m, Num b) => m b -> m b -> m b
maybeadd''' mx my = do
    x <- mx
    y <- my
    return $ x + y
