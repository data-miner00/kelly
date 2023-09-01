module Folder
    ( sum'
    , or'
    , and'
    , isAll
    , isAll'
    , length'
    , length''
    , map'
    ) where


sum' :: Num a => [a] -> a
sum' lst = foldr (+) 0 lst

or' :: [Bool] -> Bool
or' lst = foldr (||) False lst

and' :: [Bool] -> Bool
and' lst = foldr (&&) True lst

-- Whether all element in a list == e
isAll :: (Foldable t, Eq a) => a -> t a -> Bool
isAll e = foldr (\x -> (&&) $ e == x) True

isAll' :: (Foldable t, Eq a) => a -> t a -> Bool
isAll' e = foldr (\x acc -> e == x && acc) True

length' :: [a] -> Int
length' lst = foldr (\_ -> (+) 1) 0 lst

length'' :: [a] -> Int
length'' lst = foldr (const $ (+) 1) 0 lst

map' :: (a -> b) -> [a] -> [b]
map' f lst = foldr ((:) . f) [] lst

{-  Folding on trees
    1. In-order
    2. Post-order
    3. Pre-order
-}
