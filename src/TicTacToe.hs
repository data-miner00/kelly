{- Tutorial: https://www.youtube.com/watch?v=0-pOaa0dpko -}
module TicTacToe
    ( play
    , Board
    , Piece (Open, Player)
    ) where
import           Data.List (intersperse)

-- An open piece has an integer (i+index), Player piece, which is X or O
data Piece
    = Open Int
    | Player Char
    deriving Eq

instance Show Piece where
    show (Open n)   = show n
    show (Player c) = [c]

type Board = [Piece]

removeNth :: Int -> [a] -> ([a], [a])
removeNth index lst = (left, right)
  where
    (left, ys) = splitAt (index - 1) lst
    right = drop 1 ys

placePiece :: [a] -> a -> Int -> [a]
placePiece board piece index = xs ++ [piece] ++ ys
  where
    (xs, ys) = removeNth index board

pieceIsOpen :: Piece -> Bool
pieceIsOpen (Open _) = True
pieceIsOpen _        = False

openSpace :: Board -> Int -> Bool
openSpace board index
    | length board < i = False
    | pieceIsOpen $ board !! i = True
    | otherwise = False
  where
    i = index - 1

-- Given a game board, get a valid position to place a piece
getPiecePosition :: Board -> IO Int
getPiecePosition board = do
    input <- getChar

    -- If input is a single digit, return as int, otherwise get input again
    if input `elem` ['1' .. '9'] && openSpace board (read [input])
        then return (read [input])
        else do
            putStrLn "Enter an open position (1-9):"
            getPiecePosition board

showBoardLine :: Board -> [Char]
showBoardLine (a:b:c:_) = show a ++ " | " ++ show b ++ " | " ++ show c
showBoardLine _         = error "List must contain at least three elements"

boardBorder :: String
boardBorder = "\n--------\n"

showBoard :: Board -> String
showBoard board =
    concat $ intersperse boardBorder $ [top, middle, bottom]
  where
    top = showBoardLine board
    middle = showBoardLine (drop 3 board)
    bottom = showBoardLine (drop 6 board)

swapPlayers :: Char -> Char
swapPlayers 'X' = 'O'
swapPlayers 'O' = 'X'
swapPlayers _   = error "swapPlayers only accepts the character O or X"

checkBoardState :: Board -> Char -> IO ()
checkBoardState board playerChr
    | tieGame board = putStrLn "It's a tie!"
    | playerWon board (Player 'X') = putStrLn "Player X won!"
    | playerWon board (Player 'O') = putStrLn "Player O won!"
    | otherwise = play board (swapPlayers playerChr)

-- Given a board, player piece, and position on board, check if
-- the player given won vertically starting from the given position
checkWonVertically :: Board -> Piece -> Int -> Bool
checkWonVertically board player index =
    topPos == player && middlePos == player  && botPos == player
  where
    topPos = board !! index
    middlePos = board !! (index + 3)
    botPos = board !! (index + 6)

playerWonVertically :: Board -> Piece -> Bool
playerWonVertically board player = or $ map (checkWonVertically board player) [0, 1, 2]

checkWonHorizontally :: Board -> Piece -> Int -> Bool
checkWonHorizontally board player index =
    firstPos == player && secondPos == player && thirdPos == player
  where
    firstPos = board !! index
    secondPos = board !! (index + 1)
    thirdPos = board !! (index + 2)

playerWonHorizontally :: Board -> Piece -> Bool
playerWonHorizontally board player = any (checkWonHorizontally board player) [0, 3, 6]

checkWonDiagonally :: Board -> Piece -> Int -> Int -> Bool
checkWonDiagonally board player index step =
    firstPos == player && secondPos == player && thirdPos == player
  where
    firstPos = board !! index
    secondPos = board !! (index + step)
    thirdPos = board !! (index + 2 * step)

playerWonDiagonally :: Board -> Piece -> Bool
playerWonDiagonally board player = wonFirstDiagonal || wonSecondDiagonal
  where
    wonFirstDiagonal = checkWonDiagonally board player 0 4
    wonSecondDiagonal = checkWonDiagonally board player 2 2

playerWon :: Board -> Piece -> Bool
playerWon board player = wonHorizontal || wonVertical || wonDiagonal
  where
    wonHorizontal = playerWonHorizontally board player
    wonVertical = playerWonVertically board player
    wonDiagonal = playerWonDiagonally board player

tieGame :: Board -> Bool
tieGame board = all (\piece -> not (pieceIsOpen piece)) board

play :: [Piece] -> Char -> IO ()
play board playerChr = do
    putStrLn $ showBoard board
    rawChoice <- getPiecePosition board
    let newBoard = placePiece board (Player playerChr) rawChoice
    checkBoardState newBoard playerChr
