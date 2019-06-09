{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import FingerdLib

returnUsers :: Connection
            -> Socket
            -> IO ()
returnUsers dbConn soc = do

    rows <- query_ dbConn allUsers

    let usernames = map username rows
        newlineSeparated =
            T.concat $ intersperse "\n" usernames

    sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) =
    BS.concat
        ["Login: ", e username, "\t\t\t\t",
         "Name: ", e realName, "\n",
         "Directory: ", e homeDir, "\t\t\t",
         "Shell: ", e shell, "\n"]
    where e = encodeUtf8

returnUser :: Connection
            -> Socket
            -> Text
            -> IO ()
returnUser dbConn soc username = do

    maybeUser <- getUser dbConn (T.strip username)

    case maybeUser of
        Nothing -> do
            putStrLn
                ("Couldn't find matching userfor username: " ++ (show username))
            return ()
        Just user -> sendAll soc (formatUser user)

handleQuery :: Connection
            -> Socket
            -> IO ()
handleQuery dbConn soc = do

    msg <- recv soc 1024

    case msg of
        "\r\n" ->
            returnUsers dbConn soc
        name ->
            returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection
              -> Socket
              -> IO ()
handleQueries dbConn sock = forever $ do

    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"

    handleQuery dbConn soc
    Network.Socket.close soc

fingerQuery :: IO ()
fingerQuery = withSocketsDo $ do

    let port = "79"
    addrinfos <- getAddrInfo
        (Just (defaultHints
            {addrFlags =
                [AI_PASSIVE]}))
        Nothing (Just port)
    let serveraddr = head addrinfos
    sock <- socket
        (addrFamily serveraddr)
        Stream defaultProtocol
    Network.Socket.bind sock (addrAddress serveraddr)

    listen sock 1
    -- Only one connection open at a time
    conn <- open "finger.db"
    handleQueries conn sock

    SQLite.close conn
    Network.Socket.close sock

dump :: Connection -> IO ()
dump conn = do

    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])

handleInsert :: Connection
            -> Socket
            -> IO ()
handleInsert dbConn soc = do

    msg <- recv soc 1024
    let stripped = T.strip $ decodeUtf8 msg
    let fields = T.split (== ',') stripped
    if (length fields == 5) then do
        let [username', shell', homeDirectory', realName', phone'] = fields
        execute_ dbConn createUsers
        maybeUser <- getUser dbConn username'
        case maybeUser of
            Nothing -> do
                execute dbConn insertUser (Null, username', shell', homeDirectory', realName', phone')
                dump dbConn
            Just _ -> do
                execute dbConn updateUser (shell', homeDirectory', realName', phone', username')
                dump dbConn
    else
        putStrLn $ mconcat [ "Received ", show msg, ", need \"[username],[shell],[home directory],[real name],[phone]\\r\\n\"" ]

handleInserts :: Connection
              -> Socket
              -> IO ()
handleInserts dbConn sock = forever $ do

    (soc, _) <- accept sock
    putStrLn "Got connection, handling insert"

    handleInsert dbConn soc
    Network.Socket.close soc

fingerInsert :: IO ()
fingerInsert = withSocketsDo $ do

    let port = "80"
    addrinfos <- getAddrInfo
        (Just (defaultHints
            {addrFlags =
                [AI_PASSIVE]}))
        Nothing (Just port)
    let serveraddr = head addrinfos
    sock <- socket
        (addrFamily serveraddr)
        Stream defaultProtocol
    Network.Socket.bind sock (addrAddress serveraddr)

    listen sock 1
    -- Only one connection open at a time
    conn <- open "finger.db"
    handleInserts conn sock

    SQLite.close conn
    Network.Socket.close sock

main :: IO ()
main = fingerInsert
