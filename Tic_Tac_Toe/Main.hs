module Main where
-- This will ensure the program waits for the user input after printing a prompt
import System.IO (hFlush, stdout)

data Marker = X | O | Empty
  deriving (Eq, Show)

data V3 a = V3 a a a
    deriving (Show, Eq)

indexV3 :: Int -> V3 a -> a
indexV3 0 (V3 x _ _) = x
indexV3 1 (V3 _ y _) = y
indexV3 2 (V3 _ _ z) = z
indexV3 _ _          = error "Index out of bounds"

updateV3 :: Int -> a -> V3 a -> V3 a
updateV3 0 new (V3 _ y z) = V3 new y z
updateV3 1 new (V3 x _ z) = V3 x new z
updateV3 2 new (V3 x y _) = V3 x y new
updateV3 _ _ v            = v

{--
* Here I created a var @title@ which is a string that contains the ASCII art title for the game.
* The syntax design is from {https://patorjk.com/software/taag/}
--}
title:: String
title = unlines
    [ " _______ _        _______           _______"
    , "|__   __(_)      |__   __|         |__   __|"
    , "   | |   _  ___     | | __ _  ___     | | ___   ___"
    , "   | |  | |/ __|    | |/ _` |/ __|    | |/ _ \\ / _ \\"
    , "   | |  | | (__     | | (_| | (__     | | (_) |  __/"
    , "   |_|  |_|\\___|    |_|\\__,_|\\___|    |_|\\___/ \\___|"
    ]

main :: IO ()
main = do
    putStrLn title
    gameLoop emptyBoard X

type Board = V3 (V3 Marker)
type Pos = (Int, Int)

emptyBoard :: Board
{-- 
* Here I create a var @emptyBoard@ which is a list of 9 spaces which uses the Replicate function, representing an empty Tic Tac Toe board.
* I moved on to create a function @displayBoard which takes a board as input and prints it in a 3x3 grid format.
--}
emptyBoard = V3 (V3 Empty Empty Empty)
                (V3 Empty Empty Empty)
                (V3 Empty Empty Empty)

showMarker :: Marker -> String
showMarker X     = "X"
showMarker O     = "O"
showMarker Empty = " "

displayBoard :: Board -> IO ()
displayBoard (V3 (V3 a b c)
                 (V3 d e f)
                 (V3 g h i)) = do
    putStrLn $ showMarker a ++ " | " ++ showMarker b ++ " | " ++ showMarker c
    putStrLn "---------"
    putStrLn $ showMarker d ++ " | " ++ showMarker e ++ " | " ++ showMarker f
    putStrLn "---------"
    putStrLn $ showMarker g ++ " | " ++ showMarker h ++ " | " ++ showMarker i
{--
* Next, I created a type signature @makeMove@ which takes:
    - a Board (list of 9 characters(a - i))
    - Int the position (1-9)
    - Char the player ('X' or 'O')
    - returns a new Board with the player's move applied.
*Then Created a function @makeMove@ which has the following parameters:
    - board: the current state of the board
    - pos: the position (1-9) where the player wants to place their mark
    - player: the character ('X' or 'O') representing the player
--}
makeMove :: Board -> Pos -> Marker -> Board
makeMove (V3 r1 r2 r3) (row, col) player =
    case row of
        0 -> V3 (updateV3 col player r1) r2 r3
        1 -> V3 r1 (updateV3 col player r2) r3
        2 -> V3 r1 r2 (updateV3 col player r3)
        _ -> V3 r1 r2 r3
{--
* Next, Ive created another type signature @playerTurn@ the Board, the Char input and returns an IO Board.
* Then I created a function @playerTurn@ which prompts the player for their move, checks if the move is valid, and updates the board accordingly. If the move is invalid, it prompts the player to try again.
    The Two Functions
    1. hFLush and stdout this is to ensure the prompt is displayed before waiting for the input of the player
--}
playerTurn :: Board -> Marker -> IO Board
playerTurn board player = do 
    putStrLn $ "Player " ++ showMarker player ++ ", enter your move as row and column (0-2 0-2): "
    hFlush stdout
    input <- getLine
    let [r, c] = map read (words input) :: [Int]
    let pos = (r, c)
    -- check if the spot is empty
    if indexV3 c (indexV3 r board) == Empty
        then return (makeMove board pos player)
        else do
            putStrLn "Invalid move, try again."
            playerTurn board player
{--
* Next, I have difined a type annotation @wininingCombos which is a list of lists of Ints.
* Then, ive created a function @winingCombos@ where i have furthere defines a list of winning combinations for the Tic Tac Toe.
* If there is no winner and the board is full, it returns True indicating a draw.
--}
winingCombos :: [[Pos]]
winingCombos =
    [ [(0,0),(0,1),(0,2)]
    , [(1,0),(1,1),(1,2)]
    , [(2,0),(2,1),(2,2)]
    , [(0,0),(1,0),(2,0)]
    , [(0,1),(1,1),(2,1)]
    , [(0,2),(1,2),(2,2)]
    , [(0,0),(1,1),(2,2)]
    , [(0,2),(1,1),(2,0)]
    ]
{--
* I have defined a type signature @checkWin@  which takes a Board and returns Maybe Char this could be a 'X' or 'O' or nothing.
* Then futhered to the function by checking all posibilities of wining combinations and if there is a winner.
--}
getPos :: Board -> Pos -> Marker
getPos (V3 r1 r2 r3) (row, col) =
    case row of
        0 -> indexV3 col r1
        1 -> indexV3 col r2
        2 -> indexV3 col r3
        _ -> Empty

checkWin :: Board -> Maybe Marker
checkWin board =
    case [
        player | line <- winingCombos,
        let [a,b,c] = map (getPos board) line,
        a /= Empty && a == b && b == c,
        let player = a
        ] of
            (p:_) -> Just p
            []    -> Nothing
isDraw :: Board -> Bool
isDraw board =
    all (/= Empty) [getPos board (r,c) | r <- [0..2], c <- [0..2]]
    && checkWin board == Nothing

gameLoop :: Board -> Marker -> IO ()
gameLoop board player = do
    case checkWin board of 
        Just p -> putStrLn $ "Player " ++ showMarker p ++ " wins!"
        Nothing -> 
            if isDraw board 
                then putStrLn "It's a draw!"
                else do
                    newBoard <- playerTurn board player
                    displayBoard newBoard
                    gameLoop newBoard (if player == X then O else X)


switchPlayer :: Marker -> Marker
switchPlayer X = O
switchPlayer O = X
switchPlayer m = m