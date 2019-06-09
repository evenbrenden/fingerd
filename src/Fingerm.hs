{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket.ByteString (recv, sendAll)
import Network.Socket hiding (recv)
import qualified Data.ByteString.Char8 as BSC8
import Data.List
import System.Environment (getArgs)

format :: [String] -> String
format args = mconcat $ intersperse "," args

-- This one sends a message to fingerd and wants it to modify the DB
main :: IO ()
main = withSocketsDo $ do

    args <- getArgs
    if (length args == 5) then do
        let port = "80"
        addrinfos <- getAddrInfo
            (Just (defaultHints
                {addrFlags =
                    [AI_PASSIVE]}))
            Nothing (Just port)
        let clientaddr = head addrinfos
        sock <- socket
            (addrFamily clientaddr)
            Stream defaultProtocol
        connect sock (addrAddress clientaddr)

        sendAll sock $ BSC8.pack $ format args
        msg <- recv sock 1024
        BSC8.putStrLn msg
        close sock
    else
        putStrLn "Usage: modify username [shell home directory real name phone]"
