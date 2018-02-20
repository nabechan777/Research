module Main
    ( main
    ) where

import Database.HDBC.Types
    ( commit
    )
import Database.HDBC.Sqlite3
    ( connectSqlite3
    )
import Database.HDBC.Session
    ( handleSqlError'
    , withConnectionIO'
    )
import System.Process
import Library.InsertDB

main :: IO ()
main = do
    _ <- createProcess (proc "sqlite3" ["research.db", "< ./resources/Database/initdb.sql"])
    _ <- handleSqlError' $ withConnectionIO' (connectSqlite3 "research") $ \conn -> do
        progressInsert $ insertStudents conn
        progressInsert $ insertLectures conn
        progressInsert $ insertGrades conn
        commit conn
    return ()
    where
        progressInsert :: IO () -> IO ()
        progressInsert action = do
            putStrLn "start"
            action
            putStrLn "finished"
