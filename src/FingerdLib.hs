{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module FingerdLib where

import Control.Exception
import Data.Text (Text)
import Data.Typeable
import Database.SQLite.Simple hiding (close)
import Database.SQLite.Simple.Types
import Text.RawString.QQ

data User =
    User {
        userId :: Integer
      , username :: Text
      , shell :: Text
      , homeDirectory :: Text
      , realName :: Text
      , phone :: Text
    } deriving (Eq, Show)

instance FromRow User where
    fromRow = User <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

instance ToRow User where
    toRow (User id_ username shell homeDir realName phone) =
        toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser =
    "INSERT INTO users\
    \ VALUES(?, ?, ?, ?, ?, ?)"

updateUser :: Query
updateUser =
    "UPDATE users SET\
    \ shell = ?,\
    \ homeDirectory = ?,\
    \ realName = ?,\
    \ phone = ?\
    \ WHERE username = ?"

allUsers :: Query
allUsers =
    "SELECT * from users"

getUserQuery :: Query
getUserQuery =
    "SELECT * from users where username = ?"

data DuplicateData =
    DuplicateData
    deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow =
    (Null, Text, Text, Text, Text, Text)

getUser :: Connection
        -> Text
        -> IO (Maybe User)
getUser conn username = do

    results <-
        query conn getUserQuery (Only username)

    case results of
        [] -> return $ Nothing
        [user] -> return $ Just user
        _ -> throwIO DuplicateData
