module Folder
    ( sum'
    , or'
    , and'
    , isAll
    , isAll'
    , length'
    , length''
    , map'
    , rev
    , prefixes
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


count' :: (Foldable t, Eq a, Num b) => a -> t a -> b
count' e = foldr (\x acc -> if x == e then acc + 1 else acc) 0

{-  Folding on trees
    1. In-order
    2. Post-order
    3. Pre-order
-}

-- Fold exercise: https://www.youtube.com/watch?v=46dksIrx6jQ
rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map (x :) acc) []

prefixes' :: [a] -> [[a]]
prefixes' = tail . scanl (\acc x -> acc ++ [x]) []

prefixes'' :: [a] -> [[a]]
prefixes'' = (\(_:xs) -> xs) . foldl (\acc x -> acc ++ [last acc ++ [x]]) [[]]

lagrange :: [(Float, Float)] -> Float -> Float
lagrange xs x = foldl (\acc (xj, y) -> acc + (y * l xj)) 0 xs
  where
    l xj = foldl (
        \acc (xk, _) ->
            if xj == xk then
                acc
            else
                acc * ((x - xk) / (xj - xk))) 1 xs

lagrange' :: (Foldable t, Eq b, Fractional b) => t (b, b) -> b -> b
lagrange' k xp = foldr (\(x,y) acc -> acc + y * foldr (\(x1,_) acc1 -> if x /= x1 then acc1*(xp-x1) / (x-x1) else acc1) 1 k) 0 k

-- Prefix tree
data Trie a = Leaf a | Node a [Trie a]

t :: Trie Char
t =
    Node 'c' [
        Node 'a' [
            Leaf 'r', Leaf 't'
        ],
        Node 'o' [
            Node 'o' [
                Leaf 'l'
            ]
        ]
    ]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x) = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs
  where
    f' acc t = foldtrie f acc t
