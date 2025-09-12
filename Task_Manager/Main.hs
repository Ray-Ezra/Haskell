module Main where
import System.IO 
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

title:: String
title = unlines
    [
        "▄▖    ▌   ▖  ▖",
        "▐ ▀▌▛▘▙▘  ▛▖▞▌▀▌▛▌▀▌▛▌█▌▛▘",
        "▐ █▌▄▌▛▖  ▌▝ ▌█▌▌▌█▌▙▌▙▖▌",
        "                    ▄▌"
    ]

main :: IO ()
main = do
    putStrLn title
{--
* Here im defininig thet Task structure, This will hold the information about each task
* The object has four properties: id, title, description, and completed and since they are related to task ive used the data keyword to define a new data type called Task
--}
data Task = Task {
    taskId :: Int,
    taskTitle :: String,
    taskDescription :: String,
    taskCompleted :: Bool
} 

{--

--}
fileName :: FilePath
fileName = "tasks.db"
{--
--}
loadTasks :: IO [Task]
loadTasks = do
    existinigTasks <- doesFileExist fileName
    if not existinigTasks
        then return []
        else do 
            content <- readFile fileName
            case readMaybe content :: Maybe [Task] of
                Just ts -> return ts
                nothing -> putStrLn("Warning: could not parse"  ++ fileName ++ ", starting with an empty task list.") >> return []

saveTasks :: [Task] -> IO ()
saveTasks ts = writeFile fileName (show ts)
{--
*
--}
prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine

{--
*
--}
nextId :: [Task] -> Int
nextId [] = 1
nextId ts = maximum (map taskId ts) + 1

printTask :: Task -> IO ()
printTask t = putStrLn $ 
    show (taskId t) ++ ". [" ++ (if completed t then "x" else " ") ++ "] " ++ taskTitle t ++ (if null (taskDesc t) then "" else " - " ++ taskDesc t)

listTasks :: [Task] -> IO ()
listTasks [] = putStrLn "No tasks available."
listTasks ts = mapM_ printTask ts

{--
* Creating Tasks
--}
createTask :: [Task] -> IO [Task]
createTask ts = do
    t <- prompt "Enter task title: "
    d <- prompt "Enter task description: "
    let newTask = Task (nextId ts) t d False
    putStrLn $ "Task created with ID: " ++ show (taskId newTask)
    return (ts ++ [newTask])
{--
* Read the Tasks
--}
{--
* Update The Tasks
--}
{--
* Delete Tasks
--}