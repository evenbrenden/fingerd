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
import qualified Data.ByteString.Char8 as BSC8
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent
import FingerdLib

-- This one listens for messages from finger (Unix) and queries the DB
handleQueries :: Connection
              -> Socket
              -> IO ()
handleQueries dbConn server_sock = forever $ do

    (client_sock, _) <- accept server_sock
    putStrLn "Got connection, handling query"

    handleQuery dbConn client_sock
    dump dbConn
    Network.Socket.close client_sock

handleQuery :: Connection
            -> Socket
            -> IO ()
handleQuery dbConn client_sock = do

    msg <- recv client_sock 1024

    case msg of
        "\r\n" ->
            returnUsers dbConn client_sock
        name ->
            returnUser dbConn client_sock (decodeUtf8 name)

returnUsers :: Connection
            -> Socket
            -> IO ()
returnUsers dbConn client_sock = do

    rows <- query_ dbConn allUsers

    let usernames = map username rows
        newlineSeparated =
            T.concat $ intersperse "\n" usernames

    sendAll client_sock (encodeUtf8 newlineSeparated)

returnUser :: Connection
            -> Socket
            -> Text
            -> IO ()
returnUser dbConn client_sock username = do

    maybeUser <- getUser dbConn (T.strip username)

    case maybeUser of
        Nothing -> do
            putStrLn
                ("Couldn't find matching userfor username: " ++ (show username))
            return ()
        Just user -> sendAll client_sock (formatUser user)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) =
    BS.concat
        ["Login: ", e username, "\t\t\t\t",
         "Name: ", e realName, "\n",
         "Directory: ", e homeDir, "\t\t\t",
         "Shell: ", e shell, "\n"]
    where e = encodeUtf8

-- This one listens for messages from fingerm and modifies the DB
handleModifiers :: Connection
              -> Socket
              -> IO ()
handleModifiers dbConn server_sock = forever $ do

    (client_sock, _) <- accept server_sock
    putStrLn "Got connection, handling modify"

    handleModify dbConn client_sock
    dump dbConn
    Network.Socket.close client_sock

handleModify :: Connection
            -> Socket
            -> IO ()
handleModify dbConn client_sock = do

    msg <- recv client_sock 1024
    let stripped = T.strip $ decodeUtf8 msg
    let fields = T.split (== ',') stripped
    if (length fields == 5) then do
        let [username', shell', homeDirectory', realName', phone'] = fields
        execute_ dbConn createUsers
        maybeUser <- getUser dbConn username'
        case maybeUser of
            Nothing -> do
                execute dbConn insertUser (Null, username', shell', homeDirectory', realName', phone')
                sendAll client_sock (BS.concat [ "Inserted user ", encodeUtf8 username'])
            Just _ -> do
                execute dbConn updateUser (shell', homeDirectory', realName', phone', username')
                sendAll client_sock (BS.concat [ "Updated user ", encodeUtf8 username'])
    else do
        let reply = BS.concat [ "Received \"", msg, "\", need \"username,shell,home directory,real name,phone\"" ]
        sendAll client_sock reply
        BSC8.putStrLn reply

-- This one does both and is the program that is fingerd
type Handler = Connection -> Socket -> IO ()
type Port = String

runHandler :: Handler -> Port -> IO ()
runHandler handler port = withSocketsDo $ do

    addrinfos <- getAddrInfo
        (Just (defaultHints
            {addrFlags =
                [AI_PASSIVE]}))
        Nothing (Just port)
    let serveraddr = head addrinfos
    server_sock <- socket
        (addrFamily serveraddr)
        Stream defaultProtocol
    Network.Socket.bind server_sock (addrAddress serveraddr)

    -- Only one connection open at a time
    listen server_sock 1
    conn <- open "finger.db"
    handler conn server_sock

    SQLite.close conn
    Network.Socket.close server_sock

forkIO' :: IO () -> IO (MVar ())
forkIO' theThingToDo = do
    doneSignal <- newEmptyMVar
    _ <- forkFinally theThingToDo (\_ -> putMVar doneSignal ())
    return doneSignal

main :: IO ()
main = do
    queThread <- forkIO' $ runHandler handleQueries "79"
    modThread <- forkIO' $ runHandler handleModifiers "80"
    mapM_ takeMVar [queThread, modThread]
    runHandler handleQueries "79"
