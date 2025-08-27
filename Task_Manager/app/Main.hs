{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import System.Directory (doesFileExist)
import System.IO (appendFile)
import Control.Monad (when)
import Control.Monad (void)

loadTasks :: Window -> Element -> UI ()
loadTasks window list = do
    tasks <- liftIO $ lines <$> readFile todoFile
    void $ element list # set children []
    items <- mapM (fmap element . renderTask window list) (zip [0..] tasks)
    void $ element list #+ items

renderTask :: Window -> Element -> (Int, String) -> UI Element
renderTask window list (i, task) = do
    li <- UI.li
    taskSpan <- UI.span #+ [string task]
    deleteBtn <- UI.button
    updateBtn <- UI.button


    -- This are Modal dialog elements for updating a task

    modalOverlay <- UI.div # set UI.style
        [ ("position", "fixed")
        , ("top", "0")
        , ("left", "0")
        , ("width", "100%")
        , ("height", "100%")
        , ("background", "rgba(0, 0, 0, 0.5)")
        , ("display", "none")
        , ("justify-content", "center")
        , ("align-items", "center")
        ]

    modalContent <- UI.div # set UI.style
        [ ("background", "#fff")
        , ("padding", "20px")
        , ("border-radius", "8px")
        , ("min-width", "300px")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("gap", "10px")
        ]

    modalInput <- UI.input # set value task
    modalSave <- UI.button #+ [string "Save"]
    modalCancel <- UI.button #+ [string "Cancel"]

    void $ element modalSave # set UI.style [("padding", "10px"), ("background", "#3b82f6"), ("color", "#fff"), ("border", "none"), ("border-radius", "4px")]
    void $ element modalCancel # set UI.style [("padding", "10px"), ("background", "#e5e7eb"), ("border", "none"), ("border-radius", "4px")]

    void $ element modalContent #+ [element modalInput, element modalSave, element modalCancel]
    void $ element modalOverlay #+ [element modalContent]

    checkbox <- UI.input # set UI.type_ "checkbox" # set UI.style [("margin-right", "10px")]
    buttonGroup <- UI.div # set UI.style
        [ ("margin-left", "auto")
        , ("display", "flex")
        , ("gap", "10px")
        ]
        #+ [element updateBtn, element deleteBtn]

    void $ element updateBtn # set UI.style
        [ ("background", "none"), ("border", "none"), ("cursor", "pointer"), ("font-size", "18px") ]
        # set text "✏️"

    void $ element deleteBtn # set UI.style
        [ ("background", "none"), ("border", "none"), ("cursor", "pointer"), ("font-size", "18px") ]
        # set text "🗑️"

    on UI.click deleteBtn $ \_ -> do
        tasks <- liftIO $ lines <$> readFile todoFile
        let updated = unlines $ filter (/= task) tasks
        liftIO $ writeFile todoFile updated
        void $ loadTasks window list

    -- Show modal when update button is clicked


    on UI.click updateBtn $ \_ -> do
        void $ element modalOverlay # set UI.style [("display", "flex")]

    -- Save updated task from modal
    on UI.click modalSave $ \_ -> do
        newVal <- get value modalInput
        tasks <- liftIO $ lines <$> readFile todoFile
        let updated = unlines $ map (\t -> if t == task then newVal else t) tasks
        liftIO $ writeFile todoFile updated
        void $ loadTasks window list
        void $ element modalOverlay # set UI.style [("display", "none")]

    -- Hide modal on cancel


    on UI.click modalCancel $ \_ -> void $ element modalOverlay # set UI.style [("display", "none")]

    void $ element li # set UI.style
        [ ("display", "flex")
        , ("align-items", "center")
        , ("justify-content", "space-between")
        , ("padding", "12px 16px")
        , ("border", "1px solid #e5e7eb")
        , ("border-radius", "8px")
        , ("margin", "10px 0")
        , ("background-color", "#ffffff")
        ]

    -- Attach modal overlay to the body

    
    void $ getBody window #+ [element modalOverlay]

    element li #+ [element checkbox, element taskSpan, element buttonGroup]
    return li

todoFile :: FilePath
todoFile = "todo.txt"

main :: IO ()
main = startGUI defaultConfig { jsStatic = Nothing, jsPort = Just 8023 } setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Haskell Todo App"

    ensureFile

    title <- UI.h1 # set text "Todo List" # set UI.style
        [ ("margin", "0")
        , ("color", "#ffffff")
        , ("font-size", "28px")
        ]

    headerBar <- UI.div # set UI.style
        [ ("width", "100%")
        , ("background-color", "#3b82f6")
        , ("padding", "20px")
        , ("border-top-left-radius", "8px")
        , ("border-top-right-radius", "8px")
        ]
        #+ [element title]

    list <- UI.ul
    void $ element list # set UI.style
        [ ("list-style", "none")
        , ("padding", "0")
        , ("margin", "0 20px 20px 20px")
        ]
    loadTasks window list
    input <- UI.input
    addButton <- UI.button

    void $ element input # set UI.style
        [ ("padding", "20px")
        , ("font-size", "18px")
        , ("border", "1px solid #d1d5db")
        , ("flex", "1")
        , ("border-top-left-radius", "8px")
        , ("border-bottom-left-radius", "8px")
        ]

    void $ element addButton # set UI.style
        [ ("padding", "0 24px")
        , ("font-size", "24px")
        , ("background-color", "#3b82f6")
        , ("color", "white")
        , ("border", "none")
        , ("border-top-right-radius", "8px")
        , ("border-bottom-right-radius", "8px")
        , ("cursor", "pointer")
        ]
        # set text "+"

    inputRow <- UI.div # set UI.style
        [ ("display", "flex")
        , ("margin", "20px")
        , ("width", "calc(100% - 40px)")
        ]
        #+ [element input, element addButton]

    container <- UI.div # set UI.style
        [ ("padding", "0")
        , ("background", "#ffffff")
        , ("border-radius", "8px")
        , ("box-shadow", "0 10px 25px rgba(0, 0, 0, 0.1)")
        , ("min-width", "500px")
        , ("overflow", "hidden")
        ]
        #+ [element headerBar, element inputRow, UI.hr, element list]

    outer <- UI.div # set UI.style
        [ ("display", "flex")
        , ("justify-content", "center")
        , ("align-items", "center")
        , ("flex-direction", "column")
        , ("height", "100vh")
        , ("font-family", "Arial, sans-serif")
        , ("background", "#f0f4f8")
        ]

    on UI.click addButton $ \_ -> do
        task <- get value input
        if null task
            then return ()
            else do
                liftIO $ appendFile todoFile (task ++ "\n")
                loadTasks window list
                void $ element input # set value ""

    void $ element outer #+ [element container]
    void $ getBody window #+ [element outer]

ensureFile :: UI ()
ensureFile = liftIO $ do
    exists <- doesFileExist todoFile
    when (not exists) $ writeFile todoFile ""