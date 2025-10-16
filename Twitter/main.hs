{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time
import Data.IORef
import Control.Monad
import System.IO

-- Basic Data Types
data User = User
  { username :: String
  , following :: [String]
  } deriving (Show, Eq)

data Tweet = Tweet
  { author :: String
  , content :: String
  , timePosted :: String
  , likes :: Int
  } deriving (Show, Eq)

type Users = IORef [User]
type Tweets = IORef [Tweet]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  users <- newIORef []
  tweets <- newIORef []
  putStrLn "=== Welcome to HaskTwit ==="
  mainMenu users tweets

-- MAIN MENU
mainMenu :: Users -> Tweets -> IO ()
mainMenu users tweets = do
  putStrLn "\n1. Sign Up"
  putStrLn "2. Log In"
  putStrLn "3. Exit"
  putStr "Choose an option: "
  choice <- getLine
  case choice of
    "1" -> signUp users tweets
    "2" -> logIn users tweets
    "3" -> putStrLn "Goodbye!"
    _   -> putStrLn "Invalid option!" >> mainMenu users tweets

-- SIGN UP
signUp :: Users -> Tweets -> IO ()
signUp users tweets = do
  putStr "Enter a username: "
  uname <- getLine
  us <- readIORef users
  if any ((== uname) . username) us
    then putStrLn "Username already exists!" >> mainMenu users tweets
    else do
      modifyIORef users (User uname [] :)
      putStrLn $ "Account created for @" ++ uname
      mainMenu users tweets

-- LOG IN
logIn :: Users -> Tweets -> IO ()
logIn users tweets = do
  putStr "Enter username: "
  uname <- getLine
  us <- readIORef users
  if any ((== uname) . username) us
    then userMenu uname users tweets
    else putStrLn "User not found!" >> mainMenu users tweets

-- USER MENU
userMenu :: String -> Users -> Tweets -> IO ()
userMenu uname users tweets = do
  putStrLn $ "\nWelcome, @" ++ uname ++ "!"
  putStrLn "1. Post Tweet"
  putStrLn "2. View Feed"
  putStrLn "3. Follow User"
  putStrLn "4. Log Out"
  putStr "Choose an option: "
  choice <- getLine
  case choice of
    "1" -> postTweet uname tweets >> userMenu uname users tweets
    "2" -> viewFeed uname users tweets >> userMenu uname users tweets
    "3" -> followUser uname users >> userMenu uname users tweets
    "4" -> mainMenu users tweets
    _   -> putStrLn "Invalid option!" >> userMenu uname users tweets

-- POST TWEET
postTweet :: String -> Tweets -> IO ()
postTweet uname tweets = do
  putStr "What's happening? "
  msg <- getLine
  time <- getZonedTime
  modifyIORef tweets (Tweet uname msg (show time) 0 :)
  putStrLn "Tweet posted!"

-- VIEW FEED
viewFeed :: String -> Users -> Tweets -> IO ()
viewFeed uname users tweets = do
  us <- readIORef users
  ts <- readIORef tweets
  let followingList = maybe [] following (findUser uname us)
  let feed = filter (\t -> author t == uname || author t `elem` followingList) ts
  if null feed
    then putStrLn "No tweets to show."
    else forM_ (reverse feed) $ \t -> do
      putStrLn $ "\n@" ++ author t ++ ": " ++ content t
      putStrLn $ "❤️ " ++ show (likes t) ++ "  |  " ++ timePosted t

-- FOLLOW USER
followUser :: String -> Users -> IO ()
followUser uname users = do
  putStr "Enter username to follow: "
  target <- getLine
  us <- readIORef users
  if uname == target
    then putStrLn "You cannot follow yourself."
    else if any ((== target) . username) us
      then do
        let updated = map (addFollow uname target) us
        writeIORef users updated
        putStrLn $ "You are now following @" ++ target
      else putStrLn "User not found."

addFollow :: String -> String -> User -> User
addFollow uname target u
  | username u == uname && target `notElem` following u = u { following = target : following u }
  | otherwise = u

-- HELPERS
findUser :: String -> [User] -> Maybe User
findUser uname = foldr (\u acc -> if username u == uname then Just u else acc) Nothing