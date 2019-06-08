module Main where

import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import qualified Data.Text as T
import System.Environment (getArgs)
import FingerdLib

dumpAndClose :: Connection -> IO ()
dumpAndClose conn = do

    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])
    SQLite.close conn

main :: IO ()
main = do

    args <- getArgs
    if (length args == 5) then do
        let [username', shell', homeDirectory', realName', phone'] = args
        conn <- open "finger.db"
        execute_ conn createUsers
        maybeUser <- getUser conn (T.strip . T.pack $ username')
        case maybeUser of
            Nothing -> do
                execute conn insertUser (Null, username', shell', homeDirectory', realName', phone')
                dumpAndClose conn
            Just _ -> do
                execute conn updateUser (shell', homeDirectory', realName', phone', username')
                dumpAndClose conn
    else
        putStrLn "Usage: modify [user name] [shell] [home directory] [real name] [phone]"
