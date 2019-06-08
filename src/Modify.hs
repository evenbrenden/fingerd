module Main where

import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite
import System.Environment (getArgs)
import FingerdLib

main :: IO ()
main = do

    args <- getArgs
    if (length args == 5) then do
        let [username', shell', homeDirectory', realName', phone'] = args
        let news = (username', shell', homeDirectory', realName', phone')
        conn <- open "finger.db"
        execute_ conn createUsers
        execute conn insertOrReplaceUser news
        rows <- query_ conn allUsers
        mapM_ print (rows :: [User])
        SQLite.close conn
    else
        putStrLn "Usage: modify [user name] [shell] [home directory] [real name] [phone]"
