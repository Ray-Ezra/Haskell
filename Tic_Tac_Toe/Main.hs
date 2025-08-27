module Main where
-- This will ensure the program waits for the user input after printing a prompt
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "Tic Tac Toe"
    displayBoard emptyBoard

type Board = [Char]

emptyBoard :: Board
{-- 
* Here I create a variable @emptyBoard@ which is a list of 9 spaces which uses the Replicate function, representing an empty Tic Tac Toe board.
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