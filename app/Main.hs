module Main where

import System.IO
import System.Directory
import Data.List
import Control.Monad (when)

todoFile :: FilePath
todoFile = "todo.txt"

main :: IO ()
main = do
    putStrLn "TODO APP - Choose a command:"
    putStrLn "1. View tasks"
    putStrLn "2. Add task"
    putStrLn "3. Remove task"
    putStrLn "4. Quit"
    command <- getLine
    case command of
        "1" -> viewTasks
        "2" -> addTask
        "3" -> removeTask
        "4" -> putStrLn "Goodbye!"
        _   -> putStrLn "Invalid command" >> main

viewTasks :: IO ()
viewTasks = do
    exists <- doesFileExist todoFile
    if not exists then writeFile todoFile "" else return ()
    contents <- readFile todoFile
    let tasks = lines contents
    putStrLn "\nYour tasks:"
    mapM_ (\(n, line) -> putStrLn $ show n ++ ". " ++ line) (zip [1..] tasks)
    putStrLn ""
    main

addTask :: IO ()
addTask = do
    putStrLn "Enter your new task:"
    task <- getLine
    appendFile todoFile (task ++ "\n")
    putStrLn "Task added!\n"
    main

removeTask :: IO ()
removeTask = do
    contents <- readFile todoFile
    let tasks = lines contents
    putStrLn "Select task number to remove:"
    mapM_ (\(n, line) -> putStrLn $ show n ++ ". " ++ line) (zip [1..] tasks)
    number <- getLine
    let idx = read number :: Int
    when (idx <= length tasks && idx > 0) $ do
        let updated = unlines $ delete (tasks !! (idx - 1)) tasks
        writeFile todoFile updated
        putStrLn "Task removed!\n"
    main