module Data
    ( Calculation
    , calc
    , PeaNum
    , four
    , incr
    , decr
    , Tree
    , tree
    ) where

data Calculation =
    Add Int Int | Sub Int Int | Mul Int Int | Div Int Int

calc :: Calculation -> Int
calc (Add x y) = x + y
calc (Sub x y) = x - y
calc (Mul x y) = x * y
calc (Div x y) = div x y

-- Recursive type
data PeaNum = Succ PeaNum | Zero

four :: PeaNum
four = Succ $ Succ $ Succ Zero

incr :: PeaNum -> PeaNum
incr = Succ

decr :: PeaNum -> PeaNum
decr (Succ n) = n

-- Polymorphic type
data Tree a = Leaf | Node (Tree a) a (Tree a)

tree :: Tree Int
tree =
    Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 Leaf)
