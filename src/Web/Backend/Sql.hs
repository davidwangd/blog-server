module Web.Backend.Sql 
    ( openDB
    )

where

import Database.SQLite.Simple

openDB :: IO Connection
openDB = open "data.sqlite3"