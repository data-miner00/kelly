module Utils
    ( pyList
    , doubleList
    , doubleListFor
    , zippp
    , evens
    , addTuples
    , addTuples'
    , elle
    , elle'
    , nub
    , isAsc
    , isAsc'
    , hasPath
    ) where

pyList :: Int -> Int -> [Int]
pyList x y
    | x > y     = []
    | x == y    = [y]
    | x < y     = x : pyList (x+1) y

-- [ <gen> | <elem> <- <list>, ..., <guard>, ...]
doubleList :: Num a => [a] -> [a]
doubleList numList =
    [ 2*x | x <- numList ]

doubleListFor :: (Num a, Ord a) => [a] -> a -> [a]
doubleListFor numList min' =
    [ 2*x | x <- numList, x > min' ]

zippp :: [a] -> [b] -> [(a, b)]
zippp list1 list2 =
    [ (x, y) | x <- list1, y <- list2 ]

evens :: [Int] -> [Int]
evens []    = []
evens (x:xs)
    | mod x 2 == 0 = x : evens xs
    | otherwise = evens xs

addTuples :: [(Int, Int)] -> [Int]
addTuples [] = []
addTuples (x:xs) = uncurry (+) x : addTuples xs

addTuples' :: [(Int, Int)] -> [Int]
addTuples' xs = [ x+y | (x,y) <- xs ]


elle :: (Eq a) => a -> [a] -> Bool
elle _ [] = False
elle a (x:xs)
    | a == x = True
    | otherwise = elle a xs

elle' :: (Eq a) => a -> [a] -> Bool
elle' _ [] = False
elle' e (x:xs) = (e == x) || elem e xs

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs)
    | elle x xs = nub xs
    | otherwise = x : nub xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [_] = True
isAsc (x:y:xs)
    | x <= y = isAsc (y:xs)
    | otherwise = False

isAsc' :: [Int] -> Bool
isAsc' [] = True
isAsc' [_] = True
isAsc' (x:y:xs) =
    (x <= y) && isAsc' (y:xs)

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y
hasPath xs x y
  | x == y      = True
  | otherwise   =
    let
        xs' = [ (n,m) | (n,m) <- xs, n /= x ]
    in
        or [ hasPath xs' m y | (n,m) <- xs, n == x ]
