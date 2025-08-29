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