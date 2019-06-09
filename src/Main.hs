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
import FingerdLib

-- This one listens for messages from finger (Unix) and queries the DB
handleQueries :: Connection
              -> Socket
              -> IO ()
handleQueries dbConn sock = forever $ do

    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"

    handleQuery dbConn soc
    dump dbConn
    Network.Socket.close soc

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

returnUsers :: Connection
            -> Socket
            -> IO ()
returnUsers dbConn soc = do

    rows <- query_ dbConn allUsers

    let usernames = map username rows
        newlineSeparated =
            T.concat $ intersperse "\n" usernames

    sendAll soc (encodeUtf8 newlineSeparated)

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
handleModifiers dbConn sock = forever $ do

    (soc, _) <- accept sock
    putStrLn "Got connection, handling modify"

    handleModify dbConn soc
    dump dbConn
    Network.Socket.close soc

handleModify :: Connection
            -> Socket
            -> IO ()
handleModify dbConn soc = do

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
                sendAll soc (BS.concat [ "Inserted user ", encodeUtf8 username'])
            Just _ -> do
                execute dbConn updateUser (shell', homeDirectory', realName', phone', username')
                sendAll soc (BS.concat [ "Updated user ", encodeUtf8 username'])
    else do
        let reply = BS.concat [ "Received \"", msg, "\", need \"username,shell,home directory,real name,phone\"" ]
        sendAll soc reply
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
    sock <- socket
        (addrFamily serveraddr)
        Stream defaultProtocol
    Network.Socket.bind sock (addrAddress serveraddr)

    -- Only one connection open at a time
    listen sock 1
    conn <- open "finger.db"
    handler conn sock

    SQLite.close conn
    Network.Socket.close sock

main :: IO ()
main = do
    -- runHandler handleModifiers "80"
    runHandler handleQueries "79"
