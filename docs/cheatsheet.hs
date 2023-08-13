let sq x = x * x

let multMax a b x = (max a b) * x

let posOrNeg x =
    if x >= 0 then
        "Positive"
    else
        "Negative"

-- Recursion
let pow2 n =
    if n == 0 then 1 else 2 * (pow2 (n - 1))

let repeatStr str n =
    if n == 0 then ""
    else str ++ (repeatString str (n-1))

let pow2 = pow2loop n 1 0
let pow2loop n x i = 0


-- List
let x = [1, 2, 3]
let empty = []
let y = 0 : x -- cons operator

let x' = 1 : (2 : (3 : []))
let x'' = 1 : 2 : 3 : []

-- Strings
let str = "abcde"
let str' = 'a' : 'b' : 'c' : 'd' : 'e' : []

-- Concat Lists (must be homogeneous)
let conn = [1, 2, 3] ++ [4, 5]

-- Accessing Lists
let h = head [1, 2, 3] -- 1
let t = tail [1, 2, 3] -- [2, 3]

let nt = null [] -- True
let nf = null [1, 2] -- False

let mul2List nums =
    if null nums then []
    else (2* (head nums)) : (mul2List (tail nums))

let removeOdd nums =
    if null nums then []
    else
        if (mod (head nums) 2) == 0
        then (head nums) : (removeOdd (tail nums))
        else removeOdd (tail nums)
-- Tuple
let headAndLength list = (head list, length list)

let first = fst (1, "hello") -- 1
let second = snd (1, "hello") -- hello

-- Pattern Matching
let fst' (a, b) = a

let null' [] = True
let null' (x : xs) = False

-- Refrain from using head and tail by using pattern matching
let head' (x : xs) = x
let head' [] = error "head of empty list"

let mul2List' [] = []
let mul2List' (x : xs) = (2 * x) : (double xs)

-- Guards
let pow2' n
    | n == 0    = 1
    | otherwise = 2 * (pow2' (n-1))

let removeOdd' [] = []
let removeOdd (x : xs)
    | mod x 2 == 0 = x : (removeOdd' xs)
    | otherwise    = removeOdd' xs

-- Case
let mul2List'' nums = case nums of
    []       -> []
    (x : xs) -> (2 * x) : (mul2List'' xs)

let anyEven nums = case (removeOdd nums) of
    []       -> False
    (x : xs) -> True

-- Let expression
let fancySeven =
    let a = 3
    in 2 * a + 1

let fancyNine =
    let x = 4
        y = 9
    in x + y

let numEven nums =
    let evenNums = removeOdd nums
    in length evenNums

-- Where binding (must be used for function definition)
let fancyNine' = x + y
    where x = 4
          y = 5

let fancyTen = 2 * (a + 1 where a = 4) --error
let fancyTen = 2 * (let a = 4 in a + 1)

-- Whitespace
-- * never use tabs
-- * must align with param


-- Lazy Function Eval
-- foo(alpha(1), beta(2))
-- foo (alpha 1) (beta 2)

let intsFrom n = n : (intsFrom (n + 1))
let ints = intsFrom 1
let isNull = null ints
let headInts = head ints
let take10 = take 10 ints
-- length ints // infinite
let evenInts = removeOdd ints
let take10Even = take 10 eventInts


-- functions
let compose f g x = f (g x)
let addd1 x = 1 + x
let mult2 x = 2 * x

let always7 x = 7
let always7' = const 7 -- function that returns same value
let severnplus5 = (const 7) + 5

-- Partial application

-- HOF
-- Map
let lenres = map length ["hello", "abc", "1234"]
let addOneTo = map (1+) [1,3,5,7]

let dbl = map (2*)

-- Filter
let notNull xs = not (null xs)

let filtered = filter notNull ["","abc","","hello", ""]
let removeOdd'' = filter isEven

map snd (filter fst [(True,1),(False,7),(True,11)]) -- [1,11]


-- Fold
let sumOf = foldl (+) 0 [1,2,3,4]

let showPlus s x = "(" ++ s ++ "+" ++ (show x) ++ ")"
let ress = foldl showPlus "0" [1,2,3,4]


let showPlus' x s = "(" ++ (show x) ++ "+" ++ s ++ ")"
let ress' = foldr showPlus "0" [1,2,3,4]


-- Zip
zip [1, 2, 3] [4, 5, 6]
zip [1, 2] [4, 5, 3, 6]
zipWith (+) [1, 2, 3] [4, 5, 6]

-- Lambda
zipWith3 (\ x y z -> x + y + z) [1,2,3] [4,5,6] [5,7,8]
map (\ x -> 2 * x) [1,2,3]

stringLength = length . show
stringLength' x = length (show x)

f $ g $ h $ k x = f (g (h (k x)))

map (\f -> f 3) [(+1), (\x -> 2*x + 3), (*2)]
map ($3) [(+1), (\x -> 2*x + 3), (*2)] -- [4,9,6]

zipWith ($) [(+1), (\x -> 2*x + 3), (*2)] [1,2,3] -- [2,7,6]

-- New type
newtype CustomerId = MakeCustomerId Int
customer :: CustomerId
customer = MakeCustomerId 1

customerToInt :: CustomerId -> Int
customerToInt (MakeCustomerId i) = i

-- Records
data Customer = MakeCustomer
    { customerId :: CustomerId
    , name       :: String
    , luckyNumber:: Int
    }

alice :: Customer
alice = MakeCustomer
    { customerId = MakeCustomerId 13
    , name       = "Alice"
    , luckyNumber= 42
    }

-- Retrieve fields
customerId alice -- MakeCustomerId 13

-- Update
sally = alice { name = "Sally", luckyNumber = 23 }

-- Algebraic Data Types
data Customer = Customer CustomerId String Int

bob :: Customer
bob = Customer (CustomerId 13) "Bob" 42


getCustomerId :: Customer -> CustomerId
getCustomerId (Customer cust_id name luckyNumber) = cust_id
getCustomerId (Customer cust_id _ _) = cust_id

data RGB = RGB Int Int Int

data StringTree = StringTree String [StringTree]

hierarchy = StringTree "C:"
                [ StringTree "Program Files" []
                , StringTree "Users"
                    [StringTree "Alice" []]
                , StringTree "Cats" []
                ]

data Bool = False | True

x :: Bool
x = False
y :: Bool
y = True

negate :: Bool -> Bool
negate True = False
negate False = True

data DialogResponse = Yes | No | Help | Quit

data MaybeInt = NoInt | JustInt Int


defaultInt :: Int -> MaybeInt -> Int
defaultInt defaultValue NoInt = defaultValue
defaultInt _ (JustInt x) = x


data StringList = EmptyStringList
                | ConsStringList String StringList

lengthStringList :: StringList -> Int
lengthStringList EmptyStringList = 0
lengthStringList (ConsStringList _ xs) = 1 + lengthStringList xs

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs


data Maybe a = Just a | Nothing
    deriving Eq

x :: Maybe Int
x = Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal Nothing = defaultValue
fromMaybe _ (Just x) = x


data List a = Empty | Cons a (List a)

data Map k a = (k, a)

-- Type class
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys)
    | x == y    = True
    | otherwise = elem x ys

colors = [RGB 255 0 0, RGB 0 255 0, RGB 0 0 255]
green = RGB 0 255 0
greenInColors = elem green colors

instance Eq RGB where
    (RGB r1 g1 b1) == (RGB r2 g2 b2) =
        (r1 == r2) && (g1 == g2) && (b1 == b2)

instance Show RGB where
    show (RGB r g b) =
        "RGB " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b)

data Maybe' a = Nothing' | Just' a

instance (Eq a) => Eq (Maybe' a) where
    Nothing' == Nothing' = True
    Nothing' == (Just' _) = False
    (Just' _) == Nothing' = False
    (Just' x) == (Just' y) = x == y


class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

x /= y = not (x == y)
x == y = not (x /= y)


instance Eq RGB where
    (RGB r1 g1 b1) == (RGB r2 g2 b2) =
        (r1 == r2) && (g1 == g2) && (b1 == b2)
    (RGB r1 g1 b1) /= (RGB r2 g2 b2) =
        (r1 /= r2) || (g1 /= g2) || (b1 /= b2)

data Point2 = Point2 Double Double
data Point3 = Point3 Double Double Double

distance2 :: Point2 -> Point2 -> Point2
distance2 (Point2 x1 y1) (Point2 x2 y2) =
    sqrt  (dx * dx + dy * dy)
    where  dx = x1 - x2
           dy = y1 - y2

pathLength2 :: [Point2] -> Double
pathLength2 [] = 0
pathLength2 (_ : []) = 0
pathLength2 (p0 : p1 : ps) =
    distance2 p0 p1 + pathLength2 (p1 : ps)

pathLength3 :: [Point3] -> Double
pathLength3 [] = 0
pathLength3 (_ : []) = 0
pathLength3 (p0 : p1 : ps) =
    distance3 p0 p1 + pathLength3 (p1 : ps)


class Measurable a where
    distance :: a -> a -> Double

instance Measurable Point2 where
    distance = distance2

instance Measurable Point3 where
    distance (Point3 x1 y1 z1) (Point3 x2 y2 z2) =
        sqrt (dx * dx + dy * dy + dz * dz)
        where dx = x1 - x2
              dy = y1 - y2
              dz = z1 - z2

instance Measurable Double where
    distance x y = abs (x - y)


pathLength3 :: Measurable a => [a] -> Double
pathLength3 [] = 0
pathLength3 (_ : []) = 0
pathLength3 (p0 : p1 : ps) =
    distance3 p0 p1 + pathLength3 (p1 : ps)

-- Subclass
class (Eq a) => Ord a where
    (<) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    compare :: a -> a -> Ordering
    max :: a -> a -> a
    mix :: a -> a -> a

greet :: IO ()
greet = do
    putStrLn "Who are you?"
    who <- getLine
    putStrLn ("Hello " ++ who)

greetForever :: IO ()
greetForever = do
    greet
    greetForever

main :: IO ()
main = greetForever

-- return is a function that compose IO action
promptInfo :: IO (String, String)
promptInfo = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn "What is your favorite color?"
    color <- getLine
    return (name, color)

main :: IO ()
main = do
    (name, color) <- promptInfo
    putStrLn ("Hello " ++ name ++ " " ++ color)

main :: IO ()
main = do
    line1 <- getLine
    line2 <- getLine
    let lines = line1 + line2
    putStrLn lines

-- File IO
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()

interact :: (String -> String) -> IO ()

-- Small program
reverseLines :: String -> String
reverseLines input =
    unlines (map reverse lines input)

main :: IO ()
main = interact reverseLines


-- Cipher
encrypt :: Char -> Char
encrypt c
    | 'A' <= c && c < 'Z' =
        toEnum (fromEnum 'A' + 1)
    | c == 'Z' = 'A'
    | otherwise = c

handleChar :: IO ()
handleChar = do
    c <- getChar
    let u = encrypt c
    putChar c

-- Lazilly real time execution
main = interact (map encrypt)
