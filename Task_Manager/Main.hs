module Main where
import System.IO 

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
* Creating Tasks
--}
createTask :: [Task] -> IO [Task]
createTask task = do
    title <- prompt "Enter task title: "
    description <- prompt "Enter task description: "
    let newTask = (nextId) title description False
    putStrLn $ "Task created with ID: " ++ show (taskId newTask)
    return (task ++ [newTask])
{--
* Read the Tasks
--}
{--
* Update The Tasks
--}
{--
* Delete Tasks
--}