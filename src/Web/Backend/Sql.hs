module Web.Backend.Sql 
    ( openDB
    , initDB
    )

where

import Database.SQLite.Simple
import Web.Backend.Data
import qualified Data.Text as T

openDB :: IO Connection
openDB = open "data.sqlite3"

createTable :: Connection -> Query -> IO ()
createTable conn q@(Query cmd) = do
    putStrLn $ "  Executing " ++ (T.unpack cmd) ++ "..."
    execute_ conn q

initDB :: IO ()
initDB = do
    putStrLn "Executing InitDB...\n"
    conn <- openDB
    createTable conn (createTableStmt (def :: User))
    createTable conn (createTableStmt (def :: Article))
    createTable conn (createTableStmt (def :: Resource))