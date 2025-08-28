module Main where
-- This will ensure the program waits for the user input after printing a prompt
import System.IO (hFlush, stdout)

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
    gameLoop emptyBoard 'X'

type Board = [Char]

emptyBoard :: Board
{-- 
* Here I create a var @emptyBoard@ which is a list of 9 spaces which uses the Replicate function, representing an empty Tic Tac Toe board.
* I moved on to create a function @displayBoard which takes a board as input and prints it in a 3x3 grid format.
--}
emptyBoard = replicate 9 ' '
displayBoard :: Board -> IO ()
displayBoard [a,b,c,d,e,f,g,h,i] = do
    putStrLn $ [a] ++  " | " ++ [b] ++ " | " ++ [c]
    putStrLn "---------"
    putStrLn $ [d] ++  " | " ++ [e] ++ " | " ++ [f]
    putStrLn "---------"
    putStrLn $ [g] ++  " | " ++ [h] ++ " | " ++ [i]
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
makeMove :: Board -> Int -> Char -> Board
makeMove board pos player =
    take (pos - 1) board ++ [player] ++ drop pos board
{--
* Next, Ive created another type signature @playerTurn@ the Board, the Char input and returns an IO Board.
* Then I created a function @playerTurn@ which prompts the player for their move, checks if the move is valid, and updates the board accordingly. If the move is invalid, it prompts the player to try again.
    The Two Functions
    1. hFLush and stdout this is to ensure the prompt is displayed before waiting for the input of the player
--}
playerTurn :: Board -> Char -> IO Board
playerTurn board player = do 
    putStrLn $ "Player " ++ [player] ++ ", enter your move (1-9): "
    hFlush stdout
    input <- getLine
    let pos = read input :: Int
    if board !! (pos - 1) == ' '
        then return (makeMove board pos player)
        else do
            putStrLn "Invalid move, try again."
            playerTurn board player
{--
* Next, I have difined a type annotation @wininingCombos which is a list of lists of Ints.
* Then, ive created a function @winingCombos@ where i have furthere defines a list of winning combinations for the Tic Tac Toe.
* If there is no winner and the board is full, it returns True indicating a draw.
--}
winingCombos :: [[Int]]
winingCombos = 
    [ [0,1,2], [3,4,5], [6,7,8] 
    , [0,3,6], [1,4,7], [2,5,8] 
    , [0,4,8], [2,4,6]           
    ]
{--
* I have defined a type signature @checkWin@  which takes a Board and returns Maybe Char this could be a 'X' or 'O' or nothing.
* Then futhered to the function by checking all posibilities of wining combinations and if there is a winner.
--}
checkWin :: Board -> Maybe Char
checkWin board =
    case [
        player | line <- winingCombos,
        let [a,b,c] = map (board !!) line,
        a /= ' ' && a == b && b == c,
        let player = a
        ] of
            (p:_) -> Just p
            []    -> Nothing
isDraw :: Board -> Bool
isDraw board = all (/= ' ') board && checkWin board == Nothing

gameLoop :: Board -> Char -> IO ()
gameLoop board player = do
    case checkWin board of 
        Just p -> putStrLn $ "Player " ++ [p] ++ " wins!"
        Nothing -> 
            if isDraw board 
                then putStrLn "It's a draw!"
                else do
                    newBoard <- playerTurn board player
                    displayBoard newBoard
                    gameLoop newBoard (if player == 'X' then 'O' else 'X')


switchPlayer :: Char -> Char
switchPlayer 'X' = 'O'
switchPlayer 'O' = 'X'
switchPlayer c = c