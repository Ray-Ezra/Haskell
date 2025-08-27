module Main where
-- This will ensure the program waits for the user input after printing a prompt
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "Tic Tac Toe"
    displayBoard emptyBoard
    newBoard <- playerTurn emptyBoard 'X'
    displayBoard newBoard

type Board = [Char]

emptyBoard :: Board
{-- 
* Here I create a var @emptyBoard@ which is a list of 9 spaces which uses the Replicate function, representing an empty Tic Tac Toe board.
* I moved on to create a function @displayBoard@ which takes a board as input and prints it in a 3x3 grid format.
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
*
--}