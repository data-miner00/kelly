module Lib
    ( someFunc
    , inRange
    , inRange'
    , inRange''
    , factorial
    , factorial'
    , factorial''
    , factorial'''
    , isZero
    ) where

someFunc :: IO ()
someFunc = do
    putStrLn "What is your name?"
    line <- getLine
    putStrLn ("someFunc: " ++ line)
    contents <- readFile "stack.yaml"
    putStrLn contents

-- Imperative inRange
inRange :: Ord p => p -> p -> p -> Bool
inRange min' max' x =
    let in_lower_bound = min' <= x
        in_upper_bound = max' >= x
     in in_lower_bound && in_upper_bound

-- Declarative inRange
inRange' :: Ord p => p -> p -> p -> Bool
inRange' min' max' x = in_lower_bound && in_upper_bound
  where
    in_lower_bound = min' <= x
    in_upper_bound = max' >= x

-- Control flow inRange
inRange'' :: Ord p => p -> p -> p -> Bool
inRange'' min' max' x =
    if in_lower_bound
        then in_upper_bound
        else False
  where
    in_lower_bound = min' <= x
    in_upper_bound = max' >= x

-- Control flow fac
factorial :: (Ord t, Num t) => t -> t
factorial x =
    if x <= 1
        then 1
        else x * factorial (x - 1)

-- Pattern matching
factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

-- Using guards
factorial'' :: Integer -> Integer
factorial'' x
    | x <= 1    = 1
    | otherwise = x * factorial'' x - 1

isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

-- Accumulators (Tail recursive)
factorial''' :: Integer -> Integer
factorial''' n = aux n 1
  where
    aux n' acc
        | n' <= 1   = acc
        | otherwise = aux (n' - 1) (n' * acc)
