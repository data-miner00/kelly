-- Not encouraged to use
module Records where

data Person = Person { name :: String
                     , age  :: Int }

greet :: Person -> [Char]
greet (Person n _) = "Hello" ++ n

-- Multiple constructor
-- Constructor can have different names
data Point =
    D2 { x :: Int, y :: Int }
  | D3 { x :: Int, y :: Int, z :: Int }
